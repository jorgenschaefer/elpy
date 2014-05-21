import unittest
import mock
import elpy.utils.pydocutils


class TestGetPydocCompletions(unittest.TestCase):
    def test_should_return_top_level_modules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions("")
        self.assertIn('sys', modules)
        self.assertIn('json', modules)
        self.assertIn('elpy', modules)

    def test_should_return_submodules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions("elpy")
        self.assertIn("elpy.rpc", modules)
        self.assertIn("elpy.server", modules)
        modules = elpy.utils.pydocutils.get_pydoc_completions("os")
        self.assertIn("os.path", modules)

    def test_should_find_objects_in_module(self):
        self.assertIn("elpy.tests.test_pydocutils.TestGetPydocCompletions",
                      elpy.utils.pydocutils.get_pydoc_completions
                      ("elpy.tests.test_pydocutils"))

    def test_should_find_attributes_of_objects(self):
        attribs = elpy.utils.pydocutils.get_pydoc_completions(
            "elpy.tests.test_pydocutils.TestGetPydocCompletions")
        self.assertIn("elpy.tests.test_pydocutils.TestGetPydocCompletions."
                      "test_should_find_attributes_of_objects",
                      attribs)

    def test_should_return_none_for_inexisting_module(self):
        self.assertEqual([],
                         elpy.utils.pydocutils.get_pydoc_completions
                         ("does_not_exist"))

    def test_should_work_for_unicode_strings(self):
        self.assertIsNotNone(elpy.utils.pydocutils.get_pydoc_completions
                             (u"sys"))

    def test_should_find_partial_completions(self):
        self.assertIn("multiprocessing",
                      elpy.utils.pydocutils.get_pydoc_completions
                      ("multiprocess"))
        self.assertIn("multiprocessing.util",
                      elpy.utils.pydocutils.get_pydoc_completions
                      ("multiprocessing.ut"))

    def test_should_ignore_trailing_dot(self):
        self.assertIn("elpy.utils",
                      elpy.utils.pydocutils.get_pydoc_completions
                      ("elpy."))


class TestGetModules(unittest.TestCase):
    def test_should_return_top_level_modules(self):
        modules = elpy.utils.pydocutils.get_modules()
        self.assertIn('sys', modules)
        self.assertIn('json', modules)
        self.assertIn('elpy', modules)

    def test_should_return_submodules(self):
        modules = elpy.utils.pydocutils.get_modules("elpy")
        self.assertIn("rpc", modules)
        self.assertIn("server", modules)

    @mock.patch.object(elpy.utils.pydocutils, 'safeimport')
    def test_should_catch_import_errors(self, safeimport):
        def raise_function(message):
            raise elpy.utils.pydocutils.ErrorDuringImport(message,
                                                          (None, None, None))
        safeimport.side_effect = raise_function
        self.assertEqual([], elpy.utils.pydocutils.get_modules("foo.bar"))
