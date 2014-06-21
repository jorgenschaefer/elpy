"""Tests for elpy.ropebackend."""

import mock

from elpy import ropebackend
from elpy.tests import compat
from elpy.tests.support import BackendTestCase, source_and_offset


class RopeBackendTestCase(BackendTestCase):
    def setUp(self):
        super(RopeBackendTestCase, self).setUp()
        self.backend = ropebackend.RopeBackend(self.project_root)


class TestInit(RopeBackendTestCase):
    def test_should_have_rope_as_name(self):
        self.assertEqual(self.backend.name, "rope")


class TestValidate(RopeBackendTestCase):
    def test_should_call_validate_after_timeout(self):
        with mock.patch("time.time") as t:
            t.return_value = 10
            self.backend.validate()
            with mock.patch.object(self.backend, 'project') as project:
                t.return_value = 10 + ropebackend.VALIDATE_EVERY_SECONDS + 1
                self.backend.validate()

                self.assertTrue(project.validate.called)

    def test_should_not_call_validate_before_timeout(self):
        with mock.patch("time.time") as t:
            t.return_value = 10
            self.backend.validate()
            with mock.patch.object(self.backend, 'project') as project:
                t.return_value = 10 + ropebackend.VALIDATE_EVERY_SECONDS - 1
                self.backend.validate()

                self.assertFalse(project.validate.called)


class TestRPCGetCompletions(RopeBackendTestCase):
    def test_should_call_validate(self):
        with mock.patch.object(self.backend, 'validate') as validate:
            self.backend.rpc_get_completions(None, "", 0)

            self.assertTrue(validate.called)

    def test_should_return_completions(self):
        source, offset = source_and_offset("import json\n"
                                           "json.J_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual(
            sorted([cand['suffix'] for cand in completions]),
            sorted(["SONDecoder", "SONEncoder"]))

    def test_should_not_fail_on_inexisting_file(self):
        filename = self.project_root + "/doesnotexist.py"
        self.backend.rpc_get_completions(filename,
                                         "",
                                         0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_completions(None,
                                         "",
                                         0)

    def test_should_not_fail_for_module_syntax_errors(self):
        source, offset = source_and_offset(
            "class Foo(object):\n"
            "  def bar(self):\n"
            "    foo(_|_"
            "    bar("
            "\n"
            "  def a(self):\n"
            "    pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
        )

        filename = self.project_file("test.py", source)

        actual = self.backend.rpc_get_completions(filename, source, offset)

        self.assertEquals([], actual)

    def test_should_not_fail_for_bad_indentation(self):
        source, offset = source_and_offset(
            "def foo():\n"
            "       print 23_|_\n"
            "      print 17\n")
        filename = self.project_file("test.py", source)

        actual = self.backend.rpc_get_completions(filename, source, offset)

        self.assertEquals([], actual)

    def test_should_complete_top_level_modules_for_import(self):
        source, offset = source_and_offset("import multi_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        if compat.PYTHON3:
            expected = ["processing"]
        else:
            expected = ["file", "processing"]
        self.assertEqual(sorted([cand['suffix'] for cand in completions]),
                         sorted(expected))

    def test_should_complete_packages_for_import(self):
        source, offset = source_and_offset("import threading.current_t_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual([cand['suffix'] for cand in completions],
                         ["hread"])

    def test_should_not_complete_for_import(self):
        source, offset = source_and_offset("import foo.Conf_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual([cand['suffix'] for cand in completions],
                         [])

    def test_should_not_fail_for_short_module(self):
        # This throws an error in Rope which elpy hopefully catches.
        # See #186
        source, offset = source_and_offset("from .. import foo_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        # This is strictly speaking superfluous. Just avoid an error.
        self.assertIsNotNone(completions)

    def test_should_complete_sys(self):
        source, offset = source_and_offset("import sys\nsys._|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertIn('path', [cand['suffix'] for cand in completions])


class TestRPCGetCompletionDocstring(RopeBackendTestCase):
    def test_should_return_docs(self):
        source, offset = source_and_offset("import json\n"
                                           "json.J_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        completions.sort(key=lambda p: p["name"])
        prop = completions[0]
        self.assertEqual(prop["name"], "JSONDecoder")

        docs = self.backend.rpc_get_completion_docstring("JSONDecoder")

        self.assertIn("Simple JSON", docs)

    def test_should_return_none_if_unknown(self):
        docs = self.backend.rpc_get_completion_docstring("Foo")

        self.assertIsNone(docs)


class TestRPCGetCompletionLocation(RopeBackendTestCase):
    def test_should_return_location(self):
        source, offset = source_and_offset("donaudampfschiff = 1\n"
                                           "donau_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        prop = completions[0]
        self.assertEqual(prop["name"], "donaudampfschiff")

        loc = self.backend.rpc_get_completion_location("donaudampfschiff")

        self.assertEqual((filename, 1), loc)

    def test_should_return_none_if_unknown(self):
        docs = self.backend.rpc_get_completion_location("Foo")

        self.assertIsNone(docs)


class TestRPCGetDefinition(RopeBackendTestCase):
    def test_should_call_validate(self):
        with mock.patch.object(self.backend, 'validate') as validate:
            self.backend.rpc_get_definition(None, "", 0)

            self.assertTrue(validate.called)

    def test_should_return_location_in_same_file(self):
        source, offset = source_and_offset(
            "import threading\n"
            "\n"
            "\n"
            "def other_function():\n"
            "    test_f_|_unction(1, 2)\n"
            "\n"
            "\n"
            "def test_function(a, b):\n"
            "    return a + b\n")
        if compat.PYTHON3:
            source = source.replace("(a, b)", "(b, a)")
        filename = self.project_file("test.py", "")  # Unsaved
        definition = self.backend.rpc_get_definition(filename,
                                                     source,
                                                     offset)
        self.assertEqual(definition, (filename, 71))

    def test_should_return_location_in_different_file(self):
        source1 = ("def test_function(a, b):\n"
                   "    return a + b\n")
        file1 = self.project_file("test1.py", source1)
        source2, offset = source_and_offset("from test1 import test_function\n"
                                            "test_funct_|_ion(1, 2)\n")
        file2 = self.project_file("test2.py", source2)
        definition = self.backend.rpc_get_definition(file2,
                                                     source2,
                                                     offset)
        self.assertEqual(definition, (file1, 4))

    def test_should_return_none_if_location_not_found(self):
        source, offset = source_and_offset("test_f_|_unction()\n")
        filename = self.project_file("test.py", source)
        definition = self.backend.rpc_get_definition(filename,
                                                     source,
                                                     offset)
        self.assertIsNone(definition)

    def test_should_return_none_if_outside_of_symbol(self):
        source, offset = source_and_offset("test_function(_|_)\n")
        filename = self.project_file("test.py", source)
        definition = self.backend.rpc_get_definition(filename,
                                                     source,
                                                     offset)
        self.assertIsNone(definition)

    def test_should_not_fail_on_inexisting_file(self):
        filename = self.project_root + "/doesnotexist.py"
        self.backend.rpc_get_definition(filename,
                                        "",
                                        0)

    def test_should_not_fail_on_empty_file(self):
        filename = self.project_file("test.py", "")
        self.backend.rpc_get_definition(filename,
                                        "",
                                        0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_definition(None,
                                        "",
                                        0)

    def test_should_not_fail_on_keyword(self):
        source, offset = source_and_offset(
            "_|_try:\n"
            "    pass\n"
            "except:\n"
            "    pass\n")
        filename = self.project_file("test.py", source)

        self.backend.rpc_get_definition(filename, source, offset)


class TestRPCGetCalltip(RopeBackendTestCase):
    def test_should_call_validate(self):
        with mock.patch.object(self.backend, 'validate') as validate:
            self.backend.rpc_get_calltip(None, "", 0)

            self.assertTrue(validate.called)

    def test_should_get_calltip(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        if compat.PYTHON3:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, daemon=None, *)")
        else:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, verbose=None)")
        self.assertEqual(calltip, expected)

    def test_should_get_calltip_even_after_parens(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(foo()_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        if compat.PYTHON3:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, daemon=None, *)")
        else:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, verbose=None)")
        self.assertEqual(calltip, expected)

    def test_should_get_calltip_at_closing_paren(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(_|_)")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        if compat.PYTHON3:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, daemon=None, *)")
        else:
            expected = ("threading.Thread(group=None, target=None, "
                        "name=None, args=(), kwargs=None, verbose=None)")
        self.assertEqual(calltip, expected)

    def test_should_return_none_for_bad_identifier(self):
        source, offset = source_and_offset(
            "froblgoo(_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        self.assertIsNone(calltip)

    def test_should_not_fail_on_inexisting_file(self):
        filename = self.project_root + "/doesnotexist.py"
        self.backend.rpc_get_calltip(filename,
                                     "",
                                     0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_calltip(None,
                                     "",
                                     0)

    def test_should_return_none_for_module_syntax_errors(self):
        source, offset = source_and_offset(
            "class Foo(object):\n"
            "  def bar(self):\n"
            "    foo(_|_"
            "    bar("
            "\n"
            "  def a(self):\n"
            "    pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n"
            "\n"
            "  def b(self):\n"
            "  pass\n")

        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        self.assertIsNone(calltip)

    def test_should_return_none_for_bad_indentation(self):
        source, offset = source_and_offset(
            "def foo():\n"
            "       _|_print 23\n"
            "      print 17\n")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        self.assertIsNone(calltip)

    def test_should_remove_self_argument(self):
        source, offset = source_and_offset(
            "d = dict()\n"
            "d.keys(_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        if compat.PYTHON3:
            self.assertEqual(calltip, "builtins.keys()")
        else:
            self.assertEqual(calltip, "__builtin__.keys()")

    def test_should_remove_package_prefix(self):
        source, offset = source_and_offset(
            "import multiprocessing\n"
            "q = multiprocessing.Queue()\n"
            "q.qsize(_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)
        if compat.PYTHON3 and calltip is None:
            # Bug in Rope in Python 3.4, apparently
            self.assertIsNone(calltip)
        else:
            self.assertEqual(calltip, "Queue.qsize()")


class TestRPCGetDocstring(RopeBackendTestCase):
    def test_should_call_validate(self):
        with mock.patch.object(self.backend, 'validate') as validate:
            self.backend.rpc_get_docstring(None, "", 0)

            self.assertTrue(validate.called)

    def test_should_get_docstring(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread.join_|_(")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_docstring(filename,
                                                   source,
                                                   offset)

        def first_line(s):
            return s[:s.index("\n")]

        self.assertEqual(first_line(docstring),
                         'Thread.join(self, timeout=None):')

    def test_should_return_none_for_bad_identifier(self):
        source, offset = source_and_offset(
            "froblgoo_|_(\n")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_docstring(filename,
                                                   source,
                                                   offset)
        self.assertIsNone(docstring)

    def test_should_not_fail_on_inexisting_file(self):
        filename = self.project_root + "/doesnotexist.py"
        self.backend.rpc_get_docstring(filename,
                                       "",
                                       0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_docstring(None,
                                       "",
                                       0)
