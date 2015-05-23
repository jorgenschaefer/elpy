"""Glue for the "autopep8" library.

"""
try:
    import autopep8
except ImportError:
    autopep8 = None


def fix_code(code):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    return autopep8.fix_code(code)
