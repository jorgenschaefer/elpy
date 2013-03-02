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
        # This can be a foo.bar.baz, then we want to "use" that. This
        # could also be foo.bar(), then we want to visit the call.
        components = []
        while isinstance(node, ast.Attribute):
            components.insert(0, node.attr)
            node = node.value
        if isinstance(node, ast.Name):
            varname = ".".join([node.id] + components)
            self.add("use", varname)
        else:
            self.visit(node)

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
        targets = list(node.targets)
        while targets:
            target = targets.pop(0)
            if isinstance(target, (ast.Tuple, ast.List)):
                targets.extend(target.elts)
            elif isinstance(target, ast.Name):
                self.add("create", _get_name(target))
            else:
                self.visit(target)

    def visit_Module(self, node):
        for subnode in node.body:
            self.visit(subnode)

    def visit_FunctionDef(self, node):
        for decorator in node.decorator_list:
            self.visit(decorator)
        for default in node.args.defaults:
            self.visit(default)
        self.add("create", node.name)
        self.add("push")
        argnames = list(node.args.args)
        while argnames:
            arg = argnames.pop(0)
            if isinstance(arg, ast.Tuple):
                argnames.extend(arg.elts)
            else:
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
            self.visit(decorator)
        for base in node.bases:
            self.visit(base)
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
    else:
        raise ValueError("Can't find name for class of type {}"
                         .format(type(expr).__name__))


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
