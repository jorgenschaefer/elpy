import unittest
from elpy.utils import findundefined
from elpy.tests import compat


class TestFindUndefined(unittest.TestCase):
    def test_should_return_undefined_symbols(self):
        self.assertEqual(findundefined.find_undefined
                         ("import os\n"
                          "os.path.join()\n"
                          "shutil.rmtree\n"
                          "shutil = 1"),
                         ["shutil.rmtree"])

    def test_should_not_return_duplicates(self):
        self.assertEqual(findundefined.find_undefined
                         ("sys.argv[1]\n"
                          "sys.argv[2]\n"),
                         ["sys.argv"])


class TestVariableUse(unittest.TestCase):
    def script(self, source):
        scriptgen = findundefined.VariableUse()
        return scriptgen.create_script(source)

    def test_expressions(self):
        self.assertEqual(self.script("x ; x.y ; z"),
                         [("use", "x"),
                          ("use", "x.y"),
                          ("use", "z")])

    def test_call(self):
        self.assertEqual(self.script("foo.bar(a, b=c, *d, **e)"),
                         [("use", "foo.bar"),
                          ("use", "a"),
                          ("use", "c"),
                          ("use", "d"),
                          ("use", "e")])
        self.assertEqual(self.script("open('foo').read()"),
                         [("use", "open")])

    def test_import(self):
        self.assertEqual(self.script("import foo"),
                         [("create", "foo")])
        self.assertEqual(self.script("import foo, bar"),
                         [("create", "foo"),
                          ("create", "bar")])
        self.assertEqual(self.script("import foo as f, bar as b"),
                         [("create", "f"),
                          ("create", "b")])

    def test_from_import(self):
        self.assertEqual(self.script("from x import foo"),
                         [("create", "foo")])
        self.assertEqual(self.script("from x import foo, bar"),
                         [("create", "foo"),
                          ("create", "bar")])
        self.assertEqual(self.script("from x import foo as f, bar as b"),
                         [("create", "f"),
                          ("create", "b")])

    def test_assign(self):
        self.assertEqual(self.script("a = b"),
                         [("use", "b"),
                          ("create", "a")])
        self.assertEqual(self.script("a, b = c, d"),
                         [("use", "c"),
                          ("use", "d"),
                          ("create", "a"),
                          ("create", "b")])
        self.assertEqual(self.script("a.b[c], d.e[f] = g.h[i], j.k[l]"),
                         [("use", "g.h"),
                          ("use", "i"),
                          ("use", "j.k"),
                          ("use", "l"),
                          ("use", "a.b"),
                          ("use", "c"),
                          ("use", "d.e"),
                          ("use", "f")])
        self.assertEqual(self.script("[a, b] = c, d"),
                         [("use", "c"),
                          ("use", "d"),
                          ("create", "a"),
                          ("create", "b")])
        self.assertEqual(self.script("a.b().c, d.e().f = 1, 2"),
                         [("use", "a.b"),
                          ("use", "d.e")])

    def test_functiondef(self):
        self.assertEqual(self.script("def foo():\n"
                                     "    pass\n"),
                         [("create", "foo"),
                          ("push", None),
                          ("pop", None)])
        self.assertEqual(self.script("@bar\n"
                                     "@baz\n"
                                     "def foo(a, b=c, *d, **e):\n"
                                     "    return f\n"),
                         [("use", "bar"),
                          ("use", "baz"),
                          ("use", "c"),
                          ("create", "foo"),
                          ("push", None),
                          ("create", "a"),
                          ("create", "b"),
                          ("create", "d"),
                          ("create", "e"),
                          ("use", "f"),
                          ("pop", None)])
        if not compat.PYTHON3:
            self.assertEqual(self.script("def foo((a, b)):\n"
                                         "    pass"),
                             [("create", "foo"),
                              ("push", None),
                              ("create", "a"),
                              ("create", "b"),
                              ("pop", None)])

    def test_classdef(self):
        self.assertEqual(self.script("class Foo():\n"
                                     "    pass\n"),
                         [("create", "Foo"),
                          ("push", None),
                          ("pop", None)])
        self.assertEqual(self.script("@foo\n"
                                     "@qux\n"
                                     "class Bar(baz, qux):\n"
                                     "    __metaclass__ = Fnord\n"),
                         [("use", "foo"),
                          ("use", "qux"),
                          ("use", "baz"),
                          ("use", "qux"),
                          ("create", "Bar"),
                          ("push", None),
                          ("use", "Fnord"),
                          ("create", "__metaclass__"),
                          ("pop", None)])

    def test_attribute_with_call(self):
        self.assertEqual(self.script("foo.bar(x.y, a=b.c().d).baz"),
                         [("use", "foo.bar"),
                          ("use", "x.y"),
                          ("use", "b.c")])


class TestFindUndefinedFromScript(unittest.TestCase):
    def parse(self, *script):
        return list(findundefined._find_undefined_from_script(script))

    def test_should_find_trivials(self):
        self.assertEqual(self.parse(("create", "foo"),
                                    ("use", "foo"),
                                    ("use", "bar")),
                         ["bar"])

    def test_should_forget_after_pop(self):
        self.assertEqual(self.parse(("create", "foo"),
                                    ("push", None),
                                    ("create", "bar"),
                                    ("pop", None),
                                    ("use", "bar")),
                         ["bar"])

    def test_should_know_after_push(self):
        self.assertEqual(self.parse(("create", "foo"),
                                    ("push", None),
                                    ("use", "foo")),
                         [])


class TestNotFound(unittest.TestCase):
    def test_should_find_direct_reference(self):
        self.assertFalse(findundefined._not_found("foo",
                                                  ["foo", "bar", "baz"]))
        self.assertFalse(findundefined._not_found("foo.bar",
                                                  ["foo.bar", "baz.qux"]))

    def test_should_find_prefix(self):
        self.assertFalse(findundefined._not_found("foo.bar.baz",
                                                  ["foo", "bar", "baz"]))
        self.assertFalse(findundefined._not_found("foo.bar.baz.qux",
                                                  ["foo.bar"]))
