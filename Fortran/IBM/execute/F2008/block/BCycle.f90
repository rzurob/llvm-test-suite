!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : cycle statements cycle to named block or nearest DO
!*  ADAPTED FROM               : BExit
!*
!*  DESCRIPTION
!*
!*  CYCLE statements must be in a DO loop and "jump" to the named DO construct when
!*  a name appears in the statement, and otherwise to the nearest containing DO loop.
!*  Unlike the EXIT statement, we can *only* cycle to a DO construct, and not to a
!*  BLOCK.  A seperate diagnostic test case attempts to cycle to a BLOCK.
!*
!*  We test blocks with several layers of nesting: an outer do-loop, a block, an
!*  inner do-loop, and a block within that inner do-loop.
!*  To keep things simple, we repeat this construct several times, each time cycling
!*  to a different level in the structure:
!*  1-5: from within the inner block to its end (1), to the inner DO (2), to the block
!*  containing that (3), and to the outer DO (4), as well as an cycle with no name (5)
!*  (which implicitly branches to the end of the closest containing DO loop).
!*  6-9: from the inner DO to its end (6), to the block containing that (7), and to
!*  the outer DO (8), as well as an cycle with no name (9).
!*  10-12: from the outer block to its end (10), the outer DO (11), and no name (12).
!*  13-14: from the outer DO to its end (13) and no name (14).
!*  Each test tracks where it lands by constructing a string and comparing the
!*  resulting string against the expected route.  If the route is incorrect, an
!*  appropriate STOP statement is executed.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BCycle

  implicit none
  integer :: i, j, m, n
  character(16) :: hits
  m = 10
  if (command_argument_count() < 99) m = 2
  n = m

  hits = ''
  A2: do i = 1, m
     B2: block
        C2: do j = 1, n
           D2: block
                if (i==1) cycle C2 ! goes to top of C2
                hits = 'd' // hits
           end block D2
           hits = 'c' // hits
        end do C2
        hits = 'b' // hits
     end block B2
     hits = 'a' // hits
  end do A2
  print *, 1, 2, hits
  if (hits /= 'abcdcdab        ') stop 20

  hits = ''
  A4: do i = 1, m
     B4: block
        C4: do j = 1, n
           D4: block
                if (i==1) cycle A4 ! goes to top of A4
                hits = 'd' // hits
           end block D4
           hits = 'c' // hits
        end do C4
        hits = 'b' // hits
     end block B4
     hits = 'a' // hits
  end do A4
  print *, 4, hits
  if (hits /= 'abcdcd          ') stop 40


  hits = ''
  A5: do i = 1, m
     B5: block
        C5: do j = 1, n
           D5: block
                if (i==1) cycle ! goes to top of C5
                hits = 'd' // hits
           end block D5
           hits = 'c' // hits
        end do C5
        hits = 'b' // hits
     end block B5
     hits = 'a' // hits
  end do A5
  print *, 5, hits
  if (hits /= 'abcdcdab        ') stop 50


  hits = ''
  A6: do i = 1, m
     B6: block
        C6: do j = 1, n
           D6: block
                hits = 'd' // hits
           end block D6
           if (i==1) cycle C6 ! goes to top of C6
           hits = 'c' // hits
        end do C6
        hits = 'b' // hits
     end block B6
     hits = 'a' // hits
  end do A6
  print *, 6, hits
  if (hits /= 'abcdcdabdd      ') stop 60

  hits = ''
  A8: do i = 1, m
     B8: block
        C8: do j = 1, n
           D8: block
                hits = 'd' // hits
           end block D8
           if (i==1) cycle A8 ! goes to top of A8
           hits = 'c' // hits
        end do C8
        hits = 'b' // hits
     end block B8
     hits = 'a' // hits
  end do A8
  print *, 8, hits
  if (hits /= 'abcdcdd         ') stop 80

  hits = ''
  A9: do i = 1, m
     B9: block
        C9: do j = 1, n
           D9: block
                hits = 'd' // hits
           end block D9
           if (i==1) cycle ! goes to top of C9
           hits = 'c' // hits
        end do C9
        hits = 'b' // hits
     end block B9
     hits = 'a' // hits
  end do A9
  print *, 9, hits
  if (hits /= 'abcdcdabdd      ') stop 90


  hits = ''
  A11: do i = 1, m
     B11: block
        C11: do j = 1, n
           D11: block
                hits = 'd' // hits
           end block D11
           hits = 'c' // hits
        end do C11
        if (i==1) cycle A11 ! goes to top of A11
        hits = 'b' // hits
     end block B11
     hits = 'a' // hits
  end do A11
  print *, 11, hits
  if (hits /= 'abcdcdcdcd      ') stop 110


  hits = ''
  A12: do i = 1, m
     B12: block
        C12: do j = 1, n
           D12: block
                hits = 'd' // hits
           end block D12
           hits = 'c' // hits
        end do C12
        if (i==1) cycle ! goes to top of A12
        hits = 'b' // hits
     end block B12
     hits = 'a' // hits
  end do A12
  print *, 12, hits
  if (hits /= 'abcdcdcdcd      ') stop 120

  hits = ''
  A13: do i = 1, m
     B13: block
        C13: do j = 1, n
           D13: block
                hits = 'd' // hits
           end block D13
           hits = 'c' // hits
        end do C13
        hits = 'b' // hits
     end block B13
     if (i==1) cycle A13 ! goes to top of 13
     hits = 'a' // hits
  end do A13
  print *, 13, hits
  if (hits /= 'abcdcdbcdcd     ') stop 130


  hits = ''
  A14: do i = 1, m
     B14: block
        C14: do j = 1, n
           D14: block
                hits = 'd' // hits
           end block D14
           hits = 'c' // hits
        end do C14
        hits = 'b' // hits
     end block B14
     if (i==1) cycle ! goes to top of A14
     hits = 'a' // hits
  end do A14
  print *, 14, hits
  if (hits /= 'abcdcdbcdcd     ') stop 140

end program BCycle
