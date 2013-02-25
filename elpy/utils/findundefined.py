import ast


def find_undefined(source):
    finder = VariableUse()
    script = finder.create_script(source)
    undefined = _find_undefined_from_script(script)
    return sorted(set(undefined))


class VariableUse(ast.NodeVisitor):
    def __init__(self):
        self.script = []

    def create_script(self, source):
        self.script = []
        tree = ast.parse(source)
        self.visit(tree)
        return self.script

    def add(self, op, arg=None):
        self.script.append((op, arg))

    def visit_Attribute(self, node):
        if isinstance(node.value, (ast.Attribute, ast.Name)):
            self.add("use", _get_name(node))
        else:
            # foo.bar().baz, we'at baz
            self.visit(node.value)

    def visit_Name(self, node):
        self.add("use", node.id)

    def visit_Expr(self, node):
        self.visit(node.value)

    def visit_Call(self, node):
        self.visit(node.func)
        for subnode in node.args:
            self.visit(subnode)
        for kw in node.keywords:
            self.visit(kw.value)
        if node.starargs:
            self.visit(node.starargs)
        if node.kwargs:
            self.visit(node.kwargs)

    def visit_Import(self, node):
        for alias in node.names:
            self.add("create",
                     alias.asname or
                     alias.name)

    def visit_ImportFrom(self, node):
        self.visit_Import(node)

    def visit_Assign(self, node):
        self.visit(node.value)
        for target in node.targets:
            if isinstance(target, ast.Tuple):
                for elt in target.elts:
                    self.add("create", _get_name(elt))
            else:
                self.add("create", _get_name(target))

    def visit_Module(self, node):
        for subnode in node.body:
            self.visit(subnode)

    def visit_FunctionDef(self, node):
        for decorator in node.decorator_list:
            self.add("use", _get_name(decorator))
        for name in node.args.defaults:
            self.add("use", _get_name(name))
        self.add("create", node.name)
        self.add("push")
        for arg in node.args.args:
            self.add("create", _get_name(arg))
        if node.args.vararg:
            self.add("create", node.args.vararg)
        if node.args.kwarg:
            self.add("create", node.args.kwarg)
        for subnode in node.body:
            self.visit(subnode)
        self.add("pop")

    def visit_ClassDef(self, node):
        for decorator in node.decorator_list:
            self.add("use", _get_name(decorator))
        for base in node.bases:
            self.add("use", _get_name(base))
        self.add("create", node.name)
        self.add("push")
        for subnode in node.body:
            self.visit(subnode)
        self.add("pop")

    def generic_visit(self, node):
        for subnode in ast.iter_child_nodes(node):
            self.visit(subnode)


def _get_name(expr):
    if isinstance(expr, ast.Name):
        return expr.id
    elif isinstance(expr, ast.Attribute):
        return _get_name(expr.value) + "." + expr.attr
    # Python 3 has ast.arg. For Python 2, we make a duplicate check
    # which should never be true.
    elif isinstance(expr, getattr(ast, 'arg', ast.Name)):
        return expr.arg


def _find_undefined_from_script(script):
    stack = []
    current = []
    for op, arg in script:
        if op == 'use':
            if _not_found(arg, current):
                yield arg
        elif op == 'create':
            current.append(arg)
        elif op == 'push':
            stack.append(current)
            current = list(current)  # copy
        elif op == 'pop':
            current = stack.pop()


def _not_found(var, env):
    """Return True if var nor any of its prefix is in env."""
    if var in env:
        return False
    while "." in var:
        var = var[:var.rindex(".")]
        if var in env:
            return False
    return True
