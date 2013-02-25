import unittest
import tempfile
import shutil
import os
import mock

from elpy import refactor


class RefactorTestCase(unittest.TestCase):
    def setUp(self):
        self.project_root = tempfile.mkdtemp(prefix="test-refactor-root")
        self.addCleanup(shutil.rmtree, self.project_root,
                        ignore_errors=True)

    def create_file(self, name, contents=""):
        filename = os.path.join(self.project_root, name)
        offset = contents.find("_|_")
        if offset > -1:
            contents = contents[:offset] + contents[offset + 3:]
        with open(filename, "w") as f:
            f.write(contents)
        return filename, offset


class TestGetRefactorOptions(RefactorTestCase):
    def test_should_only_return_importsmodule_if_not_on_symbol(self):
        filename, offset = self.create_file("foo.py",
                                            "import foo\n"
                                            "_|_")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset)
        self.assertTrue(all(opt['category'] in ('Imports',
                                                'Module')
                            for opt in options))
        filename, offset = self.create_file("foo.py",
                                            "_|_\n"
                                            "import foo\n")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset)
        self.assertTrue(all(opt['category'] in ('Imports',
                                                'Module')
                            for opt in options))

    def test_should_return_all_if_on_symbol(self):
        filename, offset = self.create_file("foo.py",
                                            "import _|_foo")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset)
        self.assertTrue(all(opt['category'] in ('Imports',
                                                'Method',
                                                'Module',
                                                'Symbol')
                            for opt in options))

    def test_should_return_only_region_if_endoffset(self):
        filename, offset = self.create_file("foo.py",
                                            "import foo")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset, 5)
        self.assertTrue(all(opt['category'] == 'Region'
                            for opt in options))

    def test_should_treat_from_import_special(self):
        filename, offset = self.create_file("foo.py",
                                            "import foo\n"
                                            "_|_")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset)
        self.assertFalse(any(opt['name'] == "refactor_froms_to_imports"
                             for opt in options))
        filename, offset = self.create_file("foo.py",
                                            "imp_|_ort foo")
        ref = refactor.Refactor(self.project_root, filename)
        options = ref.get_refactor_options(offset)
        self.assertTrue(any(opt['name'] == "refactor_froms_to_imports"
                            for opt in options))


class TestGetChanges(RefactorTestCase):
    def test_should_fail_if_method_is_not_refactoring(self):
        filename, offset = self.create_file("foo.py")
        ref = refactor.Refactor(self.project_root, filename)
        self.assertRaises(ValueError, ref.get_changes, "bad_name")

    def test_should_return_method_results(self):
        filename, offset = self.create_file("foo.py")
        ref = refactor.Refactor(self.project_root, filename)
        with mock.patch.object(ref, 'refactor_extract_method') as test:
            test.return_value = "Meep!"
            self.assertEqual(ref.get_changes("refactor_extract_method",
                                             1, 2),
                             "Meep!")
            test.assert_called_with(1, 2)


class TestFromsToImports(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "_|_from datetime import datetime\n"
            "\n"
            "d = datetime(2013, 4, 7)\n")
        ref = refactor.Refactor(self.project_root, filename)
        (change,) = ref.get_changes("refactor_froms_to_imports", offset)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "import datetime\n"
                         "\n"
                         "d = datetime.datetime(2013, 4, 7)\n")


class TestOrganizeImports(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "import unittest, base64\n"
            "import datetime, json\n"
            "\n"
            "obj = json.dumps(23)\n"
            "unittest.TestCase()\n")
        ref = refactor.Refactor(self.project_root, filename)
        (change,) = ref.get_changes("refactor_organize_imports")
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "import json\n"
                         "import unittest\n"
                         "\n"
                         "\n"
                         "obj = json.dumps(23)\n"
                         "unittest.TestCase()\n")


class TestModuleToPackage(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "_|_import os\n")
        ref = refactor.Refactor(self.project_root, filename)
        changes = ref.refactor_module_to_package()
        a, b, c = changes
        # Not sure why the a change is there. It's a CHANGE that
        # changes nothing...
        self.assertEqual(a['diff'], '')

        self.assertEqual(b['action'], 'create')
        self.assertEqual(b['type'], 'directory')
        self.assertEqual(b['path'], os.path.join(self.project_root, "foo"))

        self.assertEqual(c['action'], 'move')
        self.assertEqual(c['type'], 'file')
        self.assertEqual(c['source'], os.path.join(self.project_root,
                                                   "foo.py"))
        self.assertEqual(c['destination'], os.path.join(self.project_root,
                                                        "foo/__init__.py"))


class TestRenameAtPoint(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "class Foo(object):\n"
            "    def _|_foo(self):\n"
            "        return 5\n"
            "\n"
            "    def bar(self):\n"
            "        return self.foo()\n")
        file2, offset2 = self.create_file(
            "bar.py",
            "import foo\n"
            "\n"
            "\n"
            "x = foo.Foo()\n"
            "x.foo()")
        ref = refactor.Refactor(self.project_root, filename)
        first, second = ref.refactor_rename_at_point(offset, "frob")
        if first['file'] == filename:
            a, b = first, second
        else:
            a, b = second, first
        self.assertEqual(a['action'], 'change')
        self.assertEqual(a['file'], filename)
        self.assertEqual(a['contents'],
                         "class Foo(object):\n"
                         "    def frob(self):\n"
                         "        return 5\n"
                         "\n"
                         "    def bar(self):\n"
                         "        return self.frob()\n")
        self.assertEqual(b['action'], 'change')
        self.assertEqual(b['file'], file2)
        self.assertEqual(b['contents'],
                         "import foo\n"
                         "\n"
                         "\n"
                         "x = foo.Foo()\n"
                         "x.frob()")


class TestRenameCurrentModule(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "_|_import os\n")
        file2, offset = self.create_file(
            "bar.py",
            "_|_import foo\n"
            "foo.os\n")
        dest = os.path.join(self.project_root, "frob.py")
        ref = refactor.Refactor(self.project_root, filename)
        a, b = ref.refactor_rename_current_module("frob")

        self.assertEqual(a['action'], 'change')
        self.assertEqual(a['file'], file2)
        self.assertEqual(a['contents'],
                         "import frob\n"
                         "frob.os\n")

        self.assertEqual(b['action'], 'move')
        self.assertEqual(b['type'], 'file')
        self.assertEqual(b['source'], filename)
        self.assertEqual(b['destination'], dest)


class TestMoveModule(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "_|_import os\n")
        file2, offset = self.create_file(
            "bar.py",
            "_|_import foo\n"
            "foo.os\n")
        dest = os.path.join(self.project_root, "frob")
        os.mkdir(dest)
        with open(os.path.join(dest, "__init__.py"), "w") as f:
            f.write("")
        ref = refactor.Refactor(self.project_root, filename)
        a, b = ref.refactor_move_module(dest)

        self.assertEqual(a['action'], 'change')
        self.assertEqual(a['file'], file2)
        self.assertEqual(a['contents'],
                         "import frob.foo\n"
                         "frob.foo.os\n")

        self.assertEqual(b['action'], 'move')
        self.assertEqual(b['type'], 'file')
        self.assertEqual(b['source'], filename)
        self.assertEqual(b['destination'],
                         os.path.join(dest, "foo.py"))


class TestCreateInline(RefactorTestCase):
    def setUp(self):
        super(TestCreateInline, self).setUp()
        self.filename, self.offset = self.create_file(
            "foo.py",
            "def add(a, b):\n"
            "    return a + b\n"
            "\n"
            "x = _|_add(2, 3)\n"
            "y = add(17, 4)\n")

    def test_should_refactor_single_occurrenc(self):
        ref = refactor.Refactor(self.project_root, self.filename)
        (change,) = ref.refactor_create_inline(self.offset, True)

        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], self.filename)
        self.assertEqual(change['contents'],
                         "def add(a, b):\n"
                         "    return a + b\n"
                         "\n"
                         "x = 2 + 3\n"
                         "y = add(17, 4)\n")

    def test_should_refactor_all_occurrencs(self):
        ref = refactor.Refactor(self.project_root, self.filename)
        (change,) = ref.refactor_create_inline(self.offset, False)

        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], self.filename)
        self.assertEqual(change['contents'],
                         "x = 2 + 3\n"
                         "y = 17 + 4\n")


class TestExtractMethod(RefactorTestCase):
    def setUp(self):
        super(TestExtractMethod, self).setUp()
        self.filename, self.offset = self.create_file(
            "foo.py",
            "class Foo(object):\n"
            "    def spaghetti(self, a, b):\n"
            "        _|_x = a + 5\n"
            "        y = b + 23\n"
            "        return y\n")

    def test_should_refactor_local(self):
        ref = refactor.Refactor(self.project_root, self.filename)
        (change,) = ref.refactor_extract_method(self.offset, 104,
                                                "calc", False)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], self.filename)
        expected = ("class Foo(object):\n"
                    "    def spaghetti(self, a, b):\n"
                    "        return self.calc(a, b)\n"
                    "\n"
                    "    def calc(self, a, b):\n"
                    "        x = a + 5\n"
                    "        y = b + 23\n"
                    "        return y\n")
        expected2 = expected.replace("return self.calc(a, b)",
                                     "return self.calc(b, a)")
        expected2 = expected2.replace("def calc(self, a, b)",
                                      "def calc(self, b, a)")
        if change['contents'] == expected2:
            self.assertEqual(change['contents'], expected2)
        else:
            self.assertEqual(change['contents'], expected)

    def test_should_refactor_global(self):
        ref = refactor.Refactor(self.project_root, self.filename)
        (change,) = ref.refactor_extract_method(self.offset, 104,
                                                "calc", True)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], self.filename)
        expected = ("class Foo(object):\n"
                    "    def spaghetti(self, a, b):\n"
                    "        return calc(a, b)\n"
                    "\n"
                    "def calc(a, b):\n"
                    "    x = a + 5\n"
                    "    y = b + 23\n"
                    "    return y\n")
        expected2 = expected.replace("return calc(a, b)",
                                     "return calc(b, a)")
        expected2 = expected2.replace("def calc(a, b)",
                                      "def calc(b, a)")
        if change['contents'] == expected2:
            self.assertEqual(change['contents'], expected2)
        else:
            self.assertEqual(change['contents'], expected)


class TestUseFunction(RefactorTestCase):
    def test_should_refactor(self):
        filename, offset = self.create_file(
            "foo.py",
            "def _|_add_and_multiply(a, b, c):\n"
            "    temp = a + b\n"
            "    return temp * c\n"
            "\n"
            "f = 1 + 2\n"
            "g = f * 3\n")

        ref = refactor.Refactor(self.project_root, filename)
        (change,) = ref.refactor_use_function(offset)

        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "def add_and_multiply(a, b, c):\n"
                         "    temp = a + b\n"
                         "    return temp * c\n"
                         "\n"
                         "g = add_and_multiply(1, 2, 3)\n")


import elpy.utils.autoimport


class TestAddMissingImports(RefactorTestCase):
    @mock.patch.object(elpy.utils.autoimport, 'get_changes')
    def test_should_call_autoimport_get_changes(self, get_changes):
        filename, offset = self.create_file("foo.py")
        ref = refactor.Refactor(self.project_root, filename)
        ref.refactor_add_missing_imports()
        get_changes.assert_called_with(filename)
