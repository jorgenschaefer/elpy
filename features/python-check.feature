Feature: Navigation
  In order to write cleaner code
  As a user
  I want to get warning messages from a code checker

  Scenario: Direct invocation of C-c C-v
    Given I have the following Python file:
    """
    x=1+1
    """
    And I press "C-x C-s"
    When I press "C-c C-v"
    And I switch to buffer "*Python Check*"
    Then I should see "Compilation started at"
