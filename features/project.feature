Feature: Project Root Handling
  In order to work on multiple files
  As a user
  I want elpy to manage a project root

  Scenario: Set project root
    Given I have an empty Python file
    When I start an action chain
    And I press "M-x"
    And I type "elpy-set-project-root"
    And I press "RET"
    And I type "/opt/foo"
    And I press "RET"
    And I execute the action chain
    Then the project root should be "/opt/foo"
