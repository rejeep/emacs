Feature: HTML Script Src
  In order to fast and easy fetch the latest script src link to a JavaScript framework
  As an Emacs user
  I want to have a simple interface through Emacs to it

  Background:
    Given I am in buffer "*temp-buffer*"
    And the buffer is empty
    
  # TODO: Would be nice with Cucumber Scenario outlines here...
  Scenario: Pick MooTools Framework
    When I start an action chain
    And I press "M-x"
    And I type "html-script-src"
    And I press "RET"
    And I type "moo"
    And I press "RET"
    And I execute the action chain
    When I am in buffer "*temp-buffer*"
    Then I should see:
      """
      <script src='http://ajax.googleapis.com/ajax/libs/mootools/1.2.4/mootools-yui-compressed.js' type='text/javascript' charset='utf-8'></script>
      """
