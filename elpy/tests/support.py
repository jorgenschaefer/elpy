# coding: utf-8

"""Support classes and functions for the elpy test code.

Elpy uses a bit of a peculiar test setup to avoid redundancy. For the
tests of the two backends, we provide generic test cases for generic
tests and for specific callback tests.

These mixins can be included in the actual test classes. We can't add
these tests to a BackendTestCase subclass directly because the test
discovery would find them there and try to run them, which would fail.

"""

import os
import shutil
import sys
import tempfile
import unittest

from elpy.tests import compat


class BackendTestCase(unittest.TestCase):
    """Base class for backend tests.

    This class sets up a project root directory and provides an easy
    way to create files within the project root.

    """

    def setUp(self):
        """Create the project root and make sure it gets cleaned up."""
        super(BackendTestCase, self).setUp()
        self.project_root = tempfile.mkdtemp(prefix="elpy-test")
        self.addCleanup(shutil.rmtree, self.project_root, True)

    def project_file(self, relname, contents):
        """Create a file named relname within the project root.

        Write contents into that file.

        """
        full_name = os.path.join(self.project_root, relname)
        try:
            os.makedirs(os.path.dirname(full_name))
        except OSError:
            pass
        if compat.PYTHON3:
            fobj = open(full_name, "w", encoding="utf-8")
        else:
            fobj = open(full_name, "w")
        with fobj as f:
            f.write(contents)
        return full_name


class GenericRPCTests(object):
    """Generic RPC test methods.

    This is a mixin to add tests that should be run for all RPC
    methods that follow the generic (filename, source, offset) calling
    conventions.

    """
    METHOD = None

    def rpc(self, filename, source, offset):
        method = getattr(self.backend, self.METHOD)
        return method(filename, source, offset)

    def test_should_not_fail_on_inexisting_file(self):
        filename = self.project_root + "/doesnotexist.py"
        self.rpc(filename, "", 0)

    def test_should_not_fail_on_empty_file(self):
        filename = self.project_file("test.py", "")
        self.rpc(filename, "", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.rpc(None, "", 0)

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

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_bad_indentation(self):
        source, offset = source_and_offset(
            "def foo():\n"
            "       print(23)_|_\n"
            "      print(17)\n")
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    @unittest.skipIf((3, 3) <= sys.version_info < (3, 4),
                     "Bug in jedi for Python 3.3")
    def test_should_not_fail_for_relative_import(self):
        source, offset = source_and_offset(
            "from .. import foo_|_"
        )
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_on_keyword(self):
        source, offset = source_and_offset(
            "_|_try:\n"
            "    pass\n"
            "except:\n"
            "    pass\n")
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_with_bad_encoding(self):
        source, offset = source_and_offset(
            u'# coding: utf-8X_|_\n'
        )
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_with_form_feed_characters(self):
        # Bug in Jedi: jedi#424
        source, offset = source_and_offset(
            "\f\n"
            "class Test(object):_|_\n"
            "    pass"
        )
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_dictionaries_in_weird_places(self):
        # Bug in Jedi: jedi#417
        source, offset = source_and_offset(
            "import json\n"
            "\n"
            "def foo():\n"
            "    json.loads(_|_\n"
            "\n"
            "    json.load.return_value = {'foo': [],\n"
            "                              'bar': True}\n"
            "\n"
            "    c = Foo()\n"
        )
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_break_with_binary_characters_in_docstring(self):
        # Bug in Jedi: jedi#427
        template = '''\
class Foo(object):
    def __init__(self):
        """
        COMMUNITY instance that this conversion belongs to.
        DISPERSY_VERSION is the dispersy conversion identifier (on the wire version; must be one byte).
        COMMUNIY_VERSION is the community conversion identifier (on the wire version; must be one byte).

        COMMUNIY_VERSION may not be '\\x00' or '\\xff'. '\\x00' is used by the DefaultConversion until
        a proper conversion instance can be made for the Community.  '\\xff' is reserved for when
        more than one byte is needed as a version indicator.
        """
        pass


x = Foo()
x._|_
'''
        source, offset = source_and_offset(template)
        filename = self.project_file("test.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_def_without_name(self):
        # Bug jedi#429
        source, offset = source_and_offset(
            "def_|_():\n"
            "    if True:\n"
            "        return True\n"
            "    else:\n"
            "        return False\n"
        )
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_on_lambda(self):
        # Bug #272 / jedi#431, jedi#572
        source, offset = source_and_offset(
            "map(lambda_|_"
        )
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_on_literals(self):
        # Bug #314, #344 / jedi#466
        source = u'lit = u"""\\\n# -*- coding: utf-8 -*-\n"""\n'
        offset = 0
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_with_args_as_args(self):
        # Bug #347 in rope_py3k
        source, offset = source_and_offset(
            "def my_function(*args):\n"
            "    ret_|_"
        )
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_unicode_chars_in_string(self):
        # Bug #358 / jedi#482
        source = '''\
# coding: utf-8

logging.info(u"Saving «{}»...".format(title))
requests.get(u"https://web.archive.org/save/{}".format(url))
'''
        offset = 57
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_bad_escape_sequence(self):
        # Bug #360 / jedi#485
        source = r"v = '\x'"
        offset = 8
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_coding_declarations_in_strings(self):
        # Bug #314 / jedi#465 / python#22221
        source = u'lit = """\\\n# -*- coding: utf-8 -*-\n"""'
        offset = 8
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_if_root_vanishes(self):
        # Bug #353
        source, offset = source_and_offset(
            "import foo\n"
            "foo._|_"
        )
        filename = self.project_file("project.py", source)
        shutil.rmtree(self.project_root)

        self.rpc(filename, source, offset)

    # For some reason, this breaks a lot of other tests. Couldn't
    # figure out why.
    #
    # def test_should_not_fail_for_sys_path(self):
    #     # Bug #365 / jedi#486
    #     source, offset = source_and_offset(
    #         "import sys\n"
    #         "\n"
    #         "sys.path.index(_|_\n"
    #     )
    #     filename = self.project_file("project.py", source)
    #
    #     self.rpc(filename, source, offset)

    def test_should_not_fail_for_key_error(self):
        # Bug #561, #564, #570, #588, #593, #599 / jedi#572, jedi#579,
        # jedi#590
        source, offset = source_and_offset(
            "map(lambda_|_"
        )
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_for_badly_defined_global_variable(self):
        # Bug #519 / jedi#610
        source, offset = source_and_offset(
            """\
def funct1():
    global global_dict_var
    global_dict_var = dict()

def funct2():
    global global_dict_var
    q = global_dict_var.copy_|_()
    print(q)""")
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)

    def test_should_not_fail_with_mergednamesdict(self):
        # Bug #563 / jedi#589
        source, offset = source_and_offset(
            u'from email import message_|_'
        )
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)


class RPCGetCompletionsTests(GenericRPCTests):
    METHOD = "rpc_get_completions"

    def test_should_complete_builtin(self):
        source, offset = source_and_offset("o_|_")

        expected = self.BUILTINS
        actual = [cand['name'] for cand in
                  self.backend.rpc_get_completions("test.py",
                                                   source, offset)]

        for candidate in expected:
            self.assertIn(candidate, actual)

    if sys.version_info >= (3, 5):
        JSON_COMPLETIONS = ["SONDecoder", "SONEncoder", "SONDecodeError"]
    else:
        JSON_COMPLETIONS = ["SONDecoder", "SONEncoder"]

    def test_should_complete_imports(self):
        source, offset = source_and_offset("import json\n"
                                           "json.J_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual(
            sorted([cand['suffix'] for cand in completions]),
            sorted(self.JSON_COMPLETIONS))

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
        source, offset = source_and_offset("import email.mi_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual([cand['suffix'] for cand in completions],
                         ["me"])

    def test_should_not_complete_for_import(self):
        source, offset = source_and_offset("import foo.Conf_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertEqual([cand['suffix'] for cand in completions],
                         [])

    @unittest.skipIf((3, 3) <= sys.version_info < (3, 4),
                     "Bug in jedi for Python 3.3")
    def test_should_not_fail_for_short_module(self):
        source, offset = source_and_offset("from .. import foo_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertIsNotNone(completions)

    def test_should_complete_sys(self):
        source, offset = source_and_offset("import sys\nsys._|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        self.assertIn('path', [cand['suffix'] for cand in completions])

    def test_should_find_with_trailing_text(self):
        source, offset = source_and_offset(
            "import threading\nthreading.T_|_mumble mumble")

        expected = ["Thread", "ThreadError", "Timer"]
        actual = [cand['name'] for cand in
                  self.backend.rpc_get_completions("test.py", source, offset)]

        for candidate in expected:
            self.assertIn(candidate, actual)

    def test_should_find_completion_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        c = Add()\n"
            "        c.ad_|_\n")
        file2 = self.project_file("project/calculator.py", source2)
        proposals = self.backend.rpc_get_completions(file2,
                                                     source2,
                                                     offset)
        self.assertEqual(["add"],
                         [proposal["name"] for proposal in proposals])


class RPCGetCompletionDocstringTests(object):
    def test_should_return_docstring(self):
        source, offset = source_and_offset("import json\n"
                                           "json.JSONEnc_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        completions.sort(key=lambda p: p["name"])
        prop = completions[0]
        self.assertEqual(prop["name"], "JSONEncoder")

        docs = self.backend.rpc_get_completion_docstring("JSONEncoder")

        self.assertIn("Extensible JSON", docs)

    def test_should_return_none_if_unknown(self):
        docs = self.backend.rpc_get_completion_docstring("Foo")

        self.assertIsNone(docs)


class RPCGetCompletionLocationTests(object):
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


class RPCGetDefinitionTests(GenericRPCTests):
    METHOD = "rpc_get_definition"

    def test_should_return_definition_location_same_file(self):
        source, offset = source_and_offset("import threading\n"
                                           "def test_function(a, b):\n"
                                           "    return a + b\n"
                                           "\n"
                                           "test_func_|_tion(\n")
        filename = self.project_file("test.py", source)

        location = self.backend.rpc_get_definition(filename,
                                                   source,
                                                   offset)

        self.assertEqual(location[0], filename)
        # On def or on the function name
        self.assertIn(location[1], (17, 21))

    def test_should_return_location_in_same_file_if_not_saved(self):
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
        filename = self.project_file("test.py", "")

        location = self.backend.rpc_get_definition(filename,
                                                   source,
                                                   offset)

        self.assertEqual(location[0], filename)
        # def or function name
        self.assertIn(location[1], (67, 71))

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

        self.assertEqual(definition[0], file1)
        # Either on the def or on the function name
        self.assertIn(definition[1], (0, 4))

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

    def test_should_return_definition_location_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        file1 = self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        return Add_|_().add(a, b)\n")
        file2 = self.project_file("project/calculator.py", source2)

        location = self.backend.rpc_get_definition(file2,
                                                   source2,
                                                   offset)

        self.assertEqual(location[0], file1)
        # class or class name
        self.assertIn(location[1], (0, 6))

    def test_should_find_variable_definition(self):
        source, offset = source_and_offset("SOME_VALUE = 1\n"
                                           "\n"
                                           "variable = _|_SOME_VALUE\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_definition(filename,
                                                         source,
                                                         offset),
                         (filename, 0))


class RPCGetAssignmentTests(GenericRPCTests):
    METHOD = "rpc_get_assignment"

    def test_should_return_assignment_location_same_file(self):
        source, offset = source_and_offset("import threading\n"
                                           "class TestClass(object):\n"
                                           "    def __init__(self, a, b):\n"
                                           "        self.a = a\n"
                                           "        self.b = b\n"
                                           "\n"
                                           "testclass = TestClass(2, 4)"
                                           "\n"
                                           "testcl_|_ass(\n")
        filename = self.project_file("test.py", source)

        location = self.backend.rpc_get_assignment(filename,
                                                   source,
                                                   offset)

        self.assertEqual(location[0], filename)
        # On def or on the function name
        self.assertEqual(location[1], 111)

    def test_should_return_location_in_same_file_if_not_saved(self):
        source, offset = source_and_offset("import threading\n"
                                           "class TestClass(object):\n"
                                           "    def __init__(self, a, b):\n"
                                           "        self.a = a\n"
                                           "        self.b = b\n"
                                           "\n"
                                           "testclass = TestClass(2, 4)"
                                           "\n"
                                           "testcl_|_ass(\n")
        filename = self.project_file("test.py", "")

        location = self.backend.rpc_get_assignment(filename,
                                                   source,
                                                   offset)

        self.assertEqual(location[0], filename)
        # def or function name
        self.assertEqual(location[1], 111)

    def test_should_return_location_in_different_file(self):
        source1 = ("class TestClass(object):\n"
                   "    def __init__(self, a, b):\n"
                   "        self.a = a\n"
                   "        self.b = b\n"
                   "testclass = TestClass(3, 5)\n")
        file1 = self.project_file("test1.py", source1)
        source2, offset = source_and_offset("from test1 import testclass\n"
                                            "testcl_|_ass.a\n")
        file2 = self.project_file("test2.py", source2)
        # First jump goes to import statement
        assignment = self.backend.rpc_get_assignment(file2,
                                                     source2,
                                                     offset)
        # Second jump goes to test1 file
        self.assertEqual(assignment[0], file2)
        assignment = self.backend.rpc_get_assignment(file2,
                                                     source2,
                                                     assignment[1])

        self.assertEqual(assignment[0], file1)
        self.assertEqual(assignment[1], 93)

    def test_should_return_none_if_location_not_found(self):
        source, offset = source_and_offset("test_f_|_unction()\n")
        filename = self.project_file("test.py", source)

        assignment = self.backend.rpc_get_assignment(filename,
                                                     source,
                                                     offset)

        self.assertIsNone(assignment)

    def test_should_return_none_if_outside_of_symbol(self):
        source, offset = source_and_offset("testcl(_|_)ass\n")
        filename = self.project_file("test.py", source)

        assignment = self.backend.rpc_get_assignment(filename,
                                                     source,
                                                     offset)

        self.assertIsNone(assignment)

    def test_should_find_variable_assignment(self):
        source, offset = source_and_offset("SOME_VALUE = 1\n"
                                           "\n"
                                           "variable = _|_SOME_VALUE\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_assignment(filename,
                                                         source,
                                                         offset),
                         (filename, 0))


class RPCGetCalltipTests(GenericRPCTests):
    METHOD = "rpc_get_calltip"

    @unittest.skipIf(sys.version_info >= (3, 0),
                     "Bug in Jedi 0.9.0")
    def test_should_get_calltip(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(_|_")
        filename = self.project_file("test.py", source)
        calltip = self.backend.rpc_get_calltip(filename,
                                               source,
                                               offset)

        expected = self.THREAD_CALLTIP

        self.assertEqual(calltip, expected)

    @unittest.skipIf(sys.version_info >= (3, 0),
                     "Bug in Jedi 0.9.0")
    def test_should_get_calltip_even_after_parens(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(foo()_|_")
        filename = self.project_file("test.py", source)

        actual = self.backend.rpc_get_calltip(filename,
                                              source,
                                              offset)

        self.assertEqual(self.THREAD_CALLTIP, actual)

    @unittest.skipIf(sys.version_info >= (3, 0),
                     "Bug in Jedi 0.9.0")
    def test_should_get_calltip_at_closing_paren(self):
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(_|_)")
        filename = self.project_file("test.py", source)

        actual = self.backend.rpc_get_calltip(filename,
                                              source,
                                              offset)

        self.assertEqual(self.THREAD_CALLTIP, actual)

    def test_should_not_missing_attribute_get_definition(self):
        # Bug #627 / jedi#573
        source, offset = source_and_offset(
            "import threading\nthreading.Thread(_|_)")
        filename = self.project_file("test.py", source)

        self.backend.rpc_get_calltip(filename, source, offset)

    def test_should_return_none_for_bad_identifier(self):
        source, offset = source_and_offset(
            "froblgoo(_|_")
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

        actual = self.backend.rpc_get_calltip(filename,
                                              source,
                                              offset)

        self.assertEqual(self.KEYS_CALLTIP, actual)

    def test_should_remove_package_prefix(self):
        source, offset = source_and_offset(
            "import decimal\n"
            "d = decimal.Decimal('1.5')\n"
            "d.radix(_|_")
        filename = self.project_file("test.py", source)

        actual = self.backend.rpc_get_calltip(filename,
                                              source,
                                              offset)

        self.assertEqual(self.RADIX_CALLTIP, actual)

    def test_should_return_none_outside_of_all(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import thr_|_eading\n")
        calltip = self.backend.rpc_get_calltip(filename,
                                               source, offset)
        self.assertIsNone(calltip)

    def test_should_find_calltip_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        c = Add()\n"
            "        c.add(_|_\n")
        file2 = self.project_file("project/calculator.py", source2)

        actual = self.backend.rpc_get_calltip(file2,
                                              source2,
                                              offset)

        self.assertEqual(self.ADD_CALLTIP, actual)


class RPCGetDocstringTests(GenericRPCTests):
    METHOD = "rpc_get_docstring"

    def check_docstring(self, docstring):

        def first_line(s):
            return s[:s.index("\n")]

        self.assertEqual(first_line(docstring),
                         self.JSON_LOADS_DOCSTRING)

    def test_should_get_docstring(self):
        source, offset = source_and_offset(
            "import json\njson.loads_|_(")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_docstring(filename,
                                                   source,
                                                   offset)
        self.check_docstring(docstring)

    def test_should_return_none_for_bad_identifier(self):
        source, offset = source_and_offset(
            "froblgoo_|_(\n")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_docstring(filename,
                                                   source,
                                                   offset)
        self.assertIsNone(docstring)


class RPCGetOnelineDocstringTests(GenericRPCTests):
    METHOD = "rpc_get_oneline_docstring"

    def check_docstring(self, docstring):

        self.assertEqual(docstring['doc'],
                         self.JSON_LOADS_DOCSTRING)

    def check_module_docstring(self, docstring):

        self.assertEqual(docstring['doc'],
                         self.JSON_DOCSTRING)

    def test_should_get_oneline_docstring(self):
        source, offset = source_and_offset(
            "import json\njson.loads_|_(")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_oneline_docstring(filename,
                                                           source,
                                                           offset)
        self.check_docstring(docstring)

    def test_should_get_oneline_docstring_for_modules(self):
        source, offset = source_and_offset(
            "import json_|_\njson.loads(")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_oneline_docstring(filename,
                                                           source,
                                                           offset)
        self.check_module_docstring(docstring)

    def test_should_return_none_for_bad_identifier(self):
        source, offset = source_and_offset(
            "froblgoo_|_(\n")
        filename = self.project_file("test.py", source)
        docstring = self.backend.rpc_get_oneline_docstring(filename,
                                                           source,
                                                           offset)
        self.assertIsNone(docstring)


class RPCGetNamesTests(GenericRPCTests):
    METHOD = "rpc_get_names"

    def test_shouldreturn_names_in_same_file(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset(
            "def foo(x, y):\n"
            "    return x + y\n"
            "c = _|_foo(5, 2)\n")

        names = self.backend.rpc_get_names(filename,
                                           source,
                                           offset)

        self.assertEqual(names,
                         [{'name': 'foo',
                           'filename': filename,
                           'offset': 4},
                          {'name': 'x',
                           'filename': filename,
                           'offset': 8},
                          {'name': 'y',
                           'filename': filename,
                           'offset': 11},
                          {'name': 'x',
                           'filename': filename,
                           'offset': 26},
                          {'name': 'y',
                           'filename': filename,
                           'offset': 30},
                          {'name': 'c',
                           'filename': filename,
                           'offset': 32},
                          {'name': 'foo',
                           'filename': filename,
                           'offset': 36}])

    def test_should_not_fail_without_symbol(self):
        filename = self.project_file("test.py", "")

        names = self.backend.rpc_get_names(filename,
                                           "",
                                           0)

        self.assertEqual(names, [])


class RPCGetUsagesTests(GenericRPCTests):
    METHOD = "rpc_get_usages"

    def test_should_return_uses_in_same_file(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset(
            "def foo(x):\n"
            "    return _|_x + x\n")

        usages = self.backend.rpc_get_usages(filename,
                                             source,
                                             offset)

        self.assertEqual(usages,
                         [{'name': 'x',
                           'offset': 8,
                           'filename': filename},
                          {'name': 'x',
                           'filename': filename,
                           'offset': 23},
                          {'name': u'x',
                           'filename': filename,
                           'offset': 27}])

    def test_should_return_uses_in_other_file(self):
        file1 = self.project_file("file1.py", "")
        file2 = self.project_file("file2.py", "\n\n\n\n\nx = 5")
        source, offset = source_and_offset(
            "import file2\n"
            "file2._|_x\n")

        usages = self.backend.rpc_get_usages(file1,
                                             source,
                                             offset)

        self.assertEqual(usages,
                         [{'name': 'x',
                           'filename': file1,
                           'offset': 19},
                          {'name': 'x',
                           'filename': file2,
                           'offset': 5}])

    def test_should_not_fail_without_symbol(self):
        filename = self.project_file("file.py", "")

        usages = self.backend.rpc_get_usages(filename,
                                             "",
                                             0)

        self.assertEqual(usages, [])


def source_and_offset(source):
    """Return a source and offset from a source description.

    >>> source_and_offset("hello, _|_world")
    ("hello, world", 7)
    >>> source_and_offset("_|_hello, world")
    ("hello, world", 0)
    >>> source_and_offset("hello, world_|_")
    ("hello, world", 12)
    """
    offset = source.index("_|_")
    return source[:offset] + source[offset + 3:], offset
