open Past

type env = (var * type_expr) list

val infer : env -> expr -> expr * type_expr
