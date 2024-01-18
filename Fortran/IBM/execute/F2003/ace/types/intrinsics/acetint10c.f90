!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier in assignment (CHARACTER)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the array constructor using just intrinsic type names works
!*  correctly within an assignment statement.  Use different forms of type
!*  specifier, with and without "kind=".  The test is successful if the programme
!*  compiles correctly.
!*  Parts of this test seem similar to acetint01, but these test different forms
!*  of assignment, not use as an actual argument.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we test CHARACTER.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10c

  implicit none

  character    :: charr(3)
  character(2) :: char2(3)
  character(256) :: ch256(3)

  charr = (/character(kind=1):: 'a', 'b', 'c' /)
  print *, 'x 1:', charr
  charr = (/character(kind=1):: 'aa','bb','cc'/)
  print *, 'x 2:', charr
  char2 = (/character(kind=1):: 'aa','bb','cc'/)
  print *, 'x 3:', char2
  ch256 = (/character(kind=1) :: &
            "From T.S. Eliot, The Lovesong of J. Alfred Prufrock: '..Disturb the universe?~In a minute there is time~For decisions and revisions which a minute will reverse.~~For I have known them all already, known them all:--~Have known the evenings, mornings, aftern", &
            "oons,~I have measured out my life with coffee spoons;~I know the voices dying with a dying fall~Beneath the music from a farther room.~So how should I presume?~~And I have known the eyes already, known them all--~The eyes that fix you in a formulated phras", &
            "e,~And when I am formulated, sprawling on a pin,~When I am pinned and wriggling on the wall,~Then how should I begin~To spit out all the butt-ends of my days and ways?~And how should I presume?~~And I have known the arms already, known them all--~Arms that"/)
  print *, 'x 4:', ch256


  charr = (/character(len=1):: 'a', 'b', 'c' /)
  print *, 'x 5:', charr
  charr = (/character(len=2):: 'a', 'b', 'c' /)
  print *, 'x 6:', charr
  charr = (/character(len=2):: 'aa','bb','cc'/)
  print *, 'x 7:', charr
  charr = (/character(len=1):: 'aa','bb','cc'/)
  print *, 'x 8:', charr

  char2 = (/character(len=1):: 'a', 'b', 'c' /)
  print *, 'x 9:', char2
  char2 = (/character(len=2):: 'a', 'b', 'c' /)
  print *, 'x10:', char2
  char2 = (/character(len=2):: 'aa','bb','cc'/)
  print *, 'x11:', char2
  char2 = (/character(len=1):: 'aa','bb','cc'/)
  print *, 'x12:', char2

  ch256 = (/character(len=256) :: &
            " are braceleted and white and bare~[But in the lamplight, downed with light brown hair!]~Is it perfume from a dress~That makes me so digress?~Arms that lie along a table, or wrap about a shawl.~And should I then presume?~And how should I begin?~. . . . .~S", &
            "hall I say, I have gone at dusk through narrow streets~And watched the smoke that rises from the pipes~Of lonely men in shirt-sleeves, leaning out of windows? . . .~~I should have been a pair of ragged claws~Scuttling across the floors of silent seas.~~. .", &
            " . . .~~And the afternoon, the evening, sleeps so peacefully!~Smoothed by long fingers,~Asleep . . . tired . . . or it malingers,~Stretched on the floor, here beside you and me.~Should I, after tea and cakes and ices,~Have the strength to force the moment "/)
  print *, 'x13:', ch256

  charr = (/character(1):: 'a', 'b', 'c' /)
  print *, 'x14:', charr
  charr = (/character(2):: 'a', 'b', 'c' /)
  print *, 'x15:', charr
  charr = (/character(2):: 'aa','bb','cc'/)
  print *, 'x16:', charr
  charr = (/character(1):: 'aa','bb','cc'/)
  print *, 'x17:', charr

  char2 = (/character(1):: 'a', 'b', 'c' /)
  print *, 'x18:', char2
  char2 = (/character(2):: 'a', 'b', 'c' /)
  print *, 'x19:', char2
  char2 = (/character(2):: 'aa','bb','cc'/)
  print *, 'x20:', char2
  char2 = (/character(1):: 'aa','bb','cc'/)
  print *, 'x21:', char2

  ch256 = (/character(256) :: &
            "to its crisis?~But though I have wept and fasted, wept and prayed,~Though I have seen my head [grown slightly bald] brought in upon a platter,~I am no prophet--and here's no great matter;~I have seen the moment of my greatness flicker,~And I have seen the ", &
            "eternal Footman hold my coat, and snicker,~And in short, I was afraid.~~And would it have been worth it, after all,~After the cups, the marmalade, the tea,~Among the porcelain, among some talk of you and me,~Would it have been worth while,~To have bitten o", &
            "ff the matter with a smile,~To have squeezed the universe into a ball~To roll it toward some overwhelming question,~To say: ""I am Lazarus, come from the dead~Come back to tell you all, I shall tell you all""--~If one, settling a pillow by her head,~Should s"/)
  print *, 'x22:', ch256

  charr = (/character*(1):: 'a', 'b', 'c' /)
  print *, 'x23:', charr
  charr = (/character*(2):: 'a', 'b', 'c' /)
  print *, 'x24:', charr
  charr = (/character*(2):: 'aa','bb','cc'/)
  print *, 'x25:', charr
  charr = (/character*(1):: 'aa','bb','cc'/)
  print *, 'x26:', charr

  char2 = (/character*(1):: 'a', 'b', 'c' /)
  print *, 'x27:', char2
  char2 = (/character*(2):: 'a', 'b', 'c' /)
  print *, 'x28:', char2
  char2 = (/character*(2):: 'aa','bb','cc'/)
  print *, 'x29:', char2
  char2 = (/character*(1):: 'aa','bb','cc'/)
  print *, 'x30:', char2

  ch256 = (/ character*(256) :: &
            "ay: ""That is not what I meant at all.~That is not it, at all.""~~And would it have been worth it, after all,~Would it have been worth while,~After the sunsets and the dooryards and the sprinkled streets,~After the novels, after the teacups, after the skirts", &
            " that trail along the~floor--~And this, and so much more?--~It is impossible to say just what I mean!~But as if a magic lantern threw the nerves in patterns on a screen:~Would it have been worth while~If one, settling a pillow or throwing off a shawl,~And ", &
            "turning toward the window, should say:~""That is not it at all,~That is not what I meant, at all.""~~. . . . .~~No! I am not Prince Hamlet, (9) nor was meant to be;~Am an attendant lord, one that will do~To swell a progress, start a scene or two,~Advise the "/)
  print *, 'x31:', ch256

  charr = (/character(len=1,kind=1):: 'a', 'b', 'c' /)
  print *, 'x32:', charr
  charr = (/character(1,1):: 'a', 'b', 'c' /)
  print *, 'x33:', charr
  charr = (/character(1,kind=1):: 'a', 'b', 'c' /)
  print *, 'x34:', charr
  charr = (/character(kind=1):: 'a', 'b', 'c' /)
  print *, 'x35:', charr
  charr = (/character(kind=1,len=1):: 'a', 'b', 'c' /)
  print *, 'x36:', charr

  charr = (/character   :: 'a1','b2','c3'/)
  print *, 'x37:', charr
  char2 = (/character   :: 'a1','b2','c3'/)
  print *, 'x38:', char2
  ch256 = (/ character  :: &
            "prince; no doubt, an easy tool,~Deferential, glad to be of use,~Politic, cautious, and meticulous;~Full of high sentence, but a bit obtuse.~At times, indeed, almost ridiculous--~Almost, at times, the Fool.~~I grow old... I grow old...~I shall wear the bott", &
            "oms of my trousers rolled.~~Shall I part my hair behind? Do I dare to eat a peach?~I shall wear white flannel trousers, and walk upon the beach.~I have heard the mermaids singing, each to each.~~I do not think that they will sing to me.~~I have seen them r", &
            "iding seaward on the waves~Combing the white hair of the waves blown back~When the wind blows the water white and black.~~We have lingered in the chambers of the sea~By sea-girls wreathed with seaweed red and brown~Till human voices wake us, and we drown.'"/)
  print *, 'x39:', ch256

end program acetint10c
