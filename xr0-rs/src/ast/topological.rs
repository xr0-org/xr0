use std::collections::VecDeque;
use std::process;

use super::ast_function_buildgraph;
use crate::util::InsertionOrderMap;
use crate::Externals;

pub type FuncGraph = InsertionOrderMap<String, Vec<String>>;

pub fn topological_order(fname: &str, ext: &Externals) -> Vec<String> {
    let mut order = vec![];
    let g = ast_function_buildgraph(fname, ext);
    // Note: Original leaks indegree_zero.
    let mut indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while let Some(curr) = indegree_zero.pop_front() {
        order.push(curr.clone());
        for (key, v) in &g {
            if v.contains(&curr) {
                let count = indegrees.get_mut(key).unwrap();
                *count -= 1;
                if *count == 0 {
                    indegree_zero.push_back(key.to_string());
                }
            }
        }
    }
    if order.len() != indegrees.len() as usize {
        eprintln!("cycle detected in graph");
        process::exit(1);
    }
    order
}

fn calculate_indegrees(g: &FuncGraph) -> InsertionOrderMap<String, usize> {
    let mut indegrees = InsertionOrderMap::new();
    for (key, deps) in g {
        if indegrees.get(key).is_none() {
            indegrees.insert(key.to_string(), 0);
            for dep_key in deps {
                if indegrees.get(dep_key).is_none() {
                    indegrees.insert(dep_key.to_string(), 0);
                }
            }
        }
    }
    for (key, count) in &mut indegrees {
        if let Some(n_arr) = g.get(key) {
            *count += n_arr.len();
        }
    }
    indegrees
}

fn build_indegree_zero(indegrees: &InsertionOrderMap<String, usize>) -> VecDeque<String> {
    let mut indegree_zero = VecDeque::new();
    for (key, val) in indegrees {
        if *val == 0 {
            indegree_zero.push_back(key.clone());
        }
    }
    indegree_zero
}
