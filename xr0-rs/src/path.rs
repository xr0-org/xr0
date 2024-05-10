use crate::ast::{
    ast_expr_assume, ast_expr_inverted_copy, ast_function_initparams, ast_function_setupabsexec,
    AstExpr, AstFunction,
};
use crate::state::State;
use crate::util::{Error, Result};
use crate::vprint;
use crate::Externals;

// Note: In the original the destructor asserts that `path_atend(p)` and leaks both states and any
// subpaths.
pub struct Path<'a> {
    path_state: PathState,
    abstract_: Option<Box<State<'a>>>,
    actual: Option<Box<State<'a>>>,
    p_true: Option<Box<Path<'a>>>,
    p_false: Option<Box<Path<'a>>>,
    f: &'a AstFunction<'a>,
    name: String,
    ext: &'a Externals,
}

#[derive(Debug, Copy, Clone)]
pub enum PathState {
    Uninit,
    Abstract,
    Halfway,
    Actual,
    Audit,
    Split,
    AtEnd,
}

impl<'a> Path<'a> {
    //=path_create
    pub fn new(f: &'a AstFunction<'a>, name: String, ext: &'a Externals) -> Box<Self> {
        // Note: The original makes a deep copy of `f`. Surprisingly this actually makes it
        // harder, not easier, for Rust to see that the function lives long enough.
        Box::new(Path {
            path_state: PathState::Uninit,
            abstract_: None,
            actual: None,
            p_true: None,
            p_false: None,
            f,
            name,
            ext,
        })
    }

    //=path_split
    fn split(&mut self, cond: Box<AstExpr>) {
        let alt = ast_expr_inverted_copy(&cond, true);
        self.p_true = Some(self.copy_with_cond(cond));
        self.p_false = Some(self.copy_with_cond(alt));
        // Note: The original has a TODO comment to destroy abstract and actual.
        self.abstract_ = None;
        self.actual = None;
        self.path_state = PathState::Split;
    }

    //=path_copywithcond
    fn copy_with_cond(&self, cond: Box<AstExpr>) -> Box<Self> {
        let mut p = Path::new(self.f, split_name(&self.f.name, &cond), self.ext);
        p.path_state = self.path_state;
        match self.path_state {
            PathState::Abstract => {
                let state = self
                    .abstract_
                    .as_ref()
                    .unwrap()
                    .clone_with_name(p.name.clone());
                p.abstract_ = Some(state);
                if !state_assume(p.abstract_.as_mut().unwrap(), &cond) {
                    p.path_state = PathState::AtEnd;
                }
            }
            PathState::Actual => {
                let state = self
                    .abstract_
                    .as_ref()
                    .unwrap()
                    .clone_with_name(p.name.clone());
                p.abstract_ = Some(state);
                let state = self
                    .actual
                    .as_ref()
                    .unwrap()
                    .clone_with_name(p.name.clone());
                p.actual = Some(state);
                if !state_assume(p.actual.as_mut().unwrap(), &cond) {
                    p.path_state = PathState::AtEnd;
                }
            }
            _ => panic!(),
        }
        p
    }

    //=path_atend
    pub fn at_end(&self) -> bool {
        match self.path_state {
            PathState::Split => {
                self.p_true.as_ref().unwrap().at_end() && self.p_false.as_ref().unwrap().at_end()
            }
            PathState::AtEnd => true,
            _ => false,
        }
    }

    //=path_step
    pub fn step(&mut self) -> Result<()> {
        match self.path_state {
            PathState::Uninit => self.init_abstract(),
            PathState::Abstract => self.step_abstract(),
            PathState::Halfway => self.init_actual(),
            PathState::Actual => self.step_actual(),
            PathState::Audit => self.audit(),
            PathState::Split => self.step_split(),
            PathState::AtEnd => panic!(),
        }
    }

    //=path_init_abstract
    fn init_abstract(&mut self) -> Result<()> {
        self.abstract_ = Some(State::new(
            self.name.clone(),
            &self.f.abstract_,
            &self.f.ret,
            true,
            self.ext,
        ));
        ast_function_initparams(self.f, self.abstract_.as_mut().unwrap())?;
        self.path_state = PathState::Abstract;
        Ok(())
    }

    //=path_init_actual
    fn init_actual(&mut self) -> Result<()> {
        let abstract_ = self.abstract_.as_ref().unwrap();
        self.actual = Some(State::with_props(
            self.name.clone(),
            self.f.body.as_ref().unwrap(),
            &self.f.ret,
            false,
            self.ext,
            abstract_.props.clone(),
        ));
        ast_function_initparams(self.f, self.actual.as_mut().unwrap())?;
        ast_function_setupabsexec(self.f, self.actual.as_mut().unwrap())?;
        self.path_state = PathState::Actual;
        Ok(())
    }

    //=path_step_abstract
    fn step_abstract(&mut self) -> Result<()> {
        let abstract_ = self.abstract_.as_mut().unwrap();
        if abstract_.at_end() {
            self.path_state = PathState::Halfway;
            return self.step();
        }
        if let Err(err) = abstract_.step() {
            let uc = err.try_into_undecideable_cond()?;
            self.split(uc);
        }
        Ok(())
    }

    //=path_step_actual
    fn step_actual(&mut self) -> Result<()> {
        let actual = self.actual.as_mut().unwrap();
        if actual.at_end() {
            self.path_state = PathState::Audit;
            return self.step();
        }
        if let Err(err) = actual.step() {
            let uc = err.try_into_undecideable_cond()?;
            self.split(uc);
        }
        Ok(())
    }

    //=path_audit
    fn audit(&mut self) -> Result<()> {
        let actual = self.actual.as_mut().unwrap();
        if actual.has_garbage() {
            vprint!("actual: {actual}");
            return Err(Error::new(format!("{}: garbage on heap", self.f.name)));
        }
        if !actual.equals(self.abstract_.as_ref().unwrap()) {
            /* unequal states are printed by State::equals so that the user
             * can see the states with undeclared vars */
            return Err(Error::new(format!(
                "{}: actual and abstract states differ",
                self.name
            )));
        }
        self.path_state = PathState::AtEnd;
        Ok(())
    }

    //=path_step_split
    fn step_split(&mut self) -> Result<()> {
        // path_atend holds this invariant whenever this function is called
        assert!(
            !self.p_true.as_ref().unwrap().at_end() || !self.p_false.as_ref().unwrap().at_end()
        );
        self.p_true.as_mut().unwrap().try_step()?;
        self.p_false.as_mut().unwrap().try_step()
    }

    //=path_trystep
    fn try_step(&mut self) -> Result<()> {
        if self.at_end() {
            Ok(())
        } else {
            self.step()
        }
    }
}

fn state_assume(s: &mut State, cond: &AstExpr) -> bool {
    let r = ast_expr_assume(cond, s).unwrap();
    !r.is_contradiction
}

fn split_name(name: &str, assumption: &AstExpr) -> String {
    format!("{name} | {assumption}")
}
