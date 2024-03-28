" Comment
syntax region	Comment start="/\*" end="\*/"

" Constant
syntax region	String		start=+"+ skip=+\\"+ end=+"+
syntax region	Character	start=+'+ skip=+\\'+ end=+'+

" Statement
syntax keyword	Constant	NULL stdin stdout stderr
syntax keyword	Operator        sizeof
syntax keyword	Keyword	        const exit
syntax keyword	Type	        unsigned int char double void bool size_t FILE
syntax keyword	Structure	struct enum union
syntax keyword	Conditional	if else for some while switch assume setup
syntax keyword	Include	        axiom malloc free realloc clump
syntax keyword	Label		case default
syntax keyword	Boolean		true false
syntax keyword	Exception	return result undefined
syntax keyword	Typedef		typedef

" Type
syntax keyword	StorageClass	auto static
