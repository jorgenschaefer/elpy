Feature: Navigation
  In order to quickly navigate Python sources
  As a user
  I want to move in semantic blocks of Python code

  Scenario: Forward simple statement
    Given I have the following Python file:
    """
    baz = quux
    foo = bar(2, 3, fnord(4))
    """
    When I press "M-e"
    Then the cursor should be after "quux"

  Scenario: Forward complex statement
    Given I have the following Python file:
    """
    foo = bar(2, 3, fnord(4))
    baz = quux
    """
    When I press "M-e"
    Then the cursor should be after "fnord(4))"

  @no24_1 @no24_2
  Scenario: Backward simple statement
    Given I have the following Python file:
    """
    baz = quux
    foo = bar(2, 3, fnord(4))
    """
    And I go to end of buffer
    When I press "M-a"
    Then the cursor should be before "foo"

  @no24_1 @no24_2
  Scenario: Backward complex statement
    Given I have the following Python file:
    """
    foo = bar(2, 3, fnord(4))
    baz = quux
    """
    And I go to end of buffer
    When I press "M-a"
    Then the cursor should be before "baz"

  Scenario: Forward definition
    Given I have the following Python file:
    """
    import foo

    Foo = 1

    def function(x, y):
        def inner_function(z):
            return x + y + z
        return function

    class Foo(object):
        CLASSVAR = 1

        def bar(self, z):
            return function(1, 2)(z)
    """
    When I press "M-n"
    Then the cursor should be before "def function"
    When I press "M-n"
    Then the cursor should be before "def inner_function"
    When I press "M-n"
    Then the cursor should be before "class Foo"
    When I press "M-n"
    Then the cursor should be before "def bar"
    When I press "M-n"
    Then the cursor should be at end of buffer

  Scenario: Backward definition
    Given I have the following Python file:
    """
    import foo

    Foo = 1

    def function(x, y):
        def inner_function(z):
            return x + y + z
        return function

    class Foo(object):
        CLASSVAR = 1

        def bar(self, z):
            return function(1, 2)(z)
    """
    And I go to end of buffer
    When I press "M-p"
    Then the cursor should be before "def bar"
    When I press "M-p"
    Then the cursor should be before "class Foo"
    When I press "M-p"
    Then the cursor should be before "def inner_function"
    When I press "M-p"
    Then the cursor should be before "def function"
    When I press "M-p"
    Then the cursor should be before "import foo"
