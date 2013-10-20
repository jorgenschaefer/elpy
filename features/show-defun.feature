Feature: Show Defun
  In order to quickly identify the name of the current function or class
  As a user
  I want to press a key and see the function or class name

  Scenario: Show the current function
    Given I have the following Python file:
    """
    def foo(self):
        pass
    """
    And I go to word "pass"
    And I press "C-c C-q"
    Then I should see the message "foo()"

  Scenario: Show the current class and method
    Given I have the following Python file:
    """
    class Foo(object):
        def bar(self):
            pass
    """
    And I go to word "pass"
    And I press "C-c C-q"
    Then I should see the message "Foo.bar()"

  Scenario: Show nested methods
    Given I have the following Python file:
    """
    class Foo(object):
        def bar(self):
            def baz(x, y):
                return x + y
            return baz(2, 3)
    """
    And I go to word "return"
    And I press "C-c C-q"
    Then I should see the message "Foo.bar.baz()"
