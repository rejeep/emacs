Feature: Emacs

  Scenario: Done
    Given I insert:
      """
      it 'should do something', ->
        foo
      """
    And I place the cursor after the word "foo"
    When I call done
    Then I should see:
      """
      it 'should do something', (done) ->
        foo
      """
    And the cursor should be after the word "foo"
    When I call done
    Then I should see:
      """
      it 'should do something', ->
        foo
      """
    And the cursor should be after the word "foo"
