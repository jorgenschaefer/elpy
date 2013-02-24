import unittest
import mock
import elpy.utils.pydocutils


class TestGetPydocCompletions(unittest.TestCase):
    def test_should_return_top_level_modules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions()
        self.assertIn('sys', modules)
        self.assertIn('json', modules)
        self.assertIn('elpy', modules)

    def test_should_return_submodules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions("elpy")
        self.assertIn("rpc", modules)
        self.assertIn("server", modules)
        modules = elpy.utils.pydocutils.get_pydoc_completions("os")
        self.assertIn("path", modules)

    def test_should_find_objects_in_module(self):
        self.assertIn(type(self).__name__,
                      elpy.utils.pydocutils.get_pydoc_completions
                      ("elpy.tests.test_pydocutils"))

    def test_should_find_attributes_of_objects(self):
        attribs = elpy.utils.pydocutils.get_pydoc_completions(
            "elpy.tests.test_pydocutils.{0}"
            .format(type(self).__name__))
        self.assertIn("test_should_find_attributes_of_objects",
                      attribs)

    def test_should_return_none_for_inexisting_module(self):
        self.assertIsNone(elpy.utils.pydocutils.get_pydoc_completions
                          ("does_not_exist"))


class TestGetModules(unittest.TestCase):
    def test_should_return_top_level_modules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions()
        self.assertIn('sys', modules)
        self.assertIn('json', modules)
        self.assertIn('elpy', modules)

    def test_should_return_submodules(self):
        modules = elpy.utils.pydocutils.get_pydoc_completions("elpy")
        self.assertIn("rpc", modules)
        self.assertIn("server", modules)
        modules = elpy.utils.pydocutils.get_pydoc_completions("os")
        self.assertIn("path", modules)

    @mock.patch.object(elpy.utils.pydocutils, 'safeimport')
    def test_should_catch_import_errors(self, safeimport):
        def raise_function(message):
            raise elpy.utils.pydocutils.ErrorDuringImport(message,
                                                          (None, None, None))
        safeimport.side_effect = raise_function
        self.assertIsNone(elpy.utils.pydocutils.get_modules("foo.bar"))
