Feature Inferior Python
  In order to interactively test Python programs
  As a user
  I want to quickly send Python code to an inferior Python process

  Scenario: Send buffer
    Given I have the following Python file:
    """
    print "Buffer sent"
    """
    When I press "C-c C-c"
    And I wait for process output
    And I wait for process output
    And I switch to buffer "*Python*"
    Then I should see "Buffer sent"

  Scenario: Send region
    Given I have the following Python file:
    """
    print "Region sent"
    print "Outside sent"
    """
    Given transient mark mode is active
    And I go to beginning of buffer
    And I press "C-@"
    And I go to end of line
    When I press "C-c C-c"
    And I wait for process output
    And I wait for process output
    And I switch to buffer "*Python*"
    Then I should see "Region sent"
    And I should not see "Outside sent"
