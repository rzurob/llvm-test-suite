!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : exit statements exit to named block or nearest DO
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  EXIT statements must be in a DO loop and "jump" to the named construct when
!*  a name appears in the statement, and otherwise to the nearest containing DO loop.
!*  We test blocks with several layers of nesting: an outer do-loop, a block, an
!*  inner do-loop, and a block within that inner do-loop.
!*  To keep things simple, we repeat this construct several times, each time exiting
!*  to a different level in the structure:
!*  1-5: from within the inner block to its end (1), to the inner DO (2), to the block
!*  containing that (3), and to the outer DO (4), as well as an exit with no name (5)
!*  (which implicitly branches to the end of the closest containing DO loop).
!*  6-9: from the inner DO to its end (6), to the block containing that (7), and to
!*  the outer DO (8), as well as an exit with no name (9).
!*  10-12: from the outer block to its end (10), the outer DO (11), and no name (12).
!*  13-14: from the outer DO to its end (13) and no name (14).
!*  Each test tracks where it lands by constructing a string and comparing the
!*  resulting string against the expected route.  If the route is incorrect, an
!*  appropriate ERROR STOP statement is executed.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BExit
  integer :: i, j, m, n
  character(4) :: hits
  m = 10
  if (command_argument_count() < 99) m = 1
  n = m

  hits = ''
  A1: do i = 1, m
     B1: block
        C1: do j = 1, n
           D1: block
                if (i==1) exit D1
                hits = 'd' // hits
           end block D1
           hits = 'c' // hits
        end do C1
        hits = 'b' // hits
     end block B1
     hits = 'a' // hits
  end do A1
  print *, 1, hits
  if (hits /= 'abc ') error stop 10

  hits = ''
  A2: do i = 1, m
     B2: block
        C2: do j = 1, n
           D2: block
                if (i==1) exit C2
                hits = 'd' // hits
           end block D2
           hits = 'c' // hits
        end do C2
        hits = 'b' // hits
     end block B2
     hits = 'a' // hits
  end do A2
  print *, 1, 2, hits
  if (hits /= 'ab  ') error stop 20

  hits = ''
  A3: do i = 1, m
     B3: block
        C3: do j = 1, n
           D3: block
                if (i==1) exit B3
                hits = 'd' // hits
           end block D3
           hits = 'c' // hits
        end do C3
        hits = 'b' // hits
     end block B3
     hits = 'a' // hits
  end do A3
  print *, 3, hits
  if (hits /= 'a   ') error stop 30

  hits = ''
  A4: do i = 1, m
     B4: block
        C4: do j = 1, n
           D4: block
                if (i==1) exit A4
                hits = 'd' // hits
           end block D4
           hits = 'c' // hits
        end do C4
        hits = 'b' // hits
     end block B4
     hits = 'a' // hits
  end do A4
  print *, 4, hits
  if (hits /= '    ') error stop 40


  hits = ''
  A5: do i = 1, m
     B5: block
        C5: do j = 1, n
           D5: block
                if (i==1) exit
                hits = 'd' // hits
           end block D5
           hits = 'c' // hits
        end do C5
        hits = 'b' // hits
     end block B5
     hits = 'a' // hits
  end do A5
  print *, 5, hits
  if (hits /= 'ab  ') error stop 50


  hits = ''
  A6: do i = 1, m
     B6: block
        C6: do j = 1, n
           D6: block
                hits = 'd' // hits
           end block D6
           if (i==1) exit C6
           hits = 'c' // hits
        end do C6
        hits = 'b' // hits
     end block B6
     hits = 'a' // hits
  end do A6
  print *, 6, hits
  if (hits /= 'abd ') error stop 60

  hits = ''
  A7: do i = 1, m
     B7: block
        C7: do j = 1, n
           D7: block
                hits = 'd' // hits
           end block D7
           if (i==1) exit B7
           hits = 'c' // hits
        end do C7
        hits = 'b' // hits
     end block B7
     hits = 'a' // hits
  end do A7
  print *, 7, hits
  if (hits /= 'ad  ') error stop 70


  hits = ''
  A8: do i = 1, m
     B8: block
        C8: do j = 1, n
           D8: block
                hits = 'd' // hits
           end block D8
           if (i==1) exit A8
           hits = 'c' // hits
        end do C8
        hits = 'b' // hits
     end block B8
     hits = 'a' // hits
  end do A8
  print *, 8, hits
  if (hits /= 'd   ') error stop 80

  hits = ''
  A9: do i = 1, m
     B9: block
        C9: do j = 1, n
           D9: block
                hits = 'd' // hits
           end block D9
           if (i==1) exit
           hits = 'c' // hits
        end do C9
        hits = 'b' // hits
     end block B9
     hits = 'a' // hits
  end do A9
  print *, 9, hits
  if (hits /= 'abd ') error stop 90


  hits = ''
  A10: do i = 1, m
     B10: block
        C10: do j = 1, n
           D10: block
                hits = 'd' // hits
           end block D10
           hits = 'c' // hits
        end do C10
        if (i==1) exit B10
        hits = 'b' // hits
     end block B10
     hits = 'a' // hits
  end do A10
  print *, 10, hits
  if (hits /= 'acd ') error stop 100

  hits = ''
  A11: do i = 1, m
     B11: block
        C11: do j = 1, n
           D11: block
                hits = 'd' // hits
           end block D11
           hits = 'c' // hits
        end do C11
        if (i==1) exit A11
        hits = 'b' // hits
     end block B11
     hits = 'a' // hits
  end do A11
  print *, 11, hits
  if (hits /= 'cd  ') error stop 110


  hits = ''
  A12: do i = 1, m
     B12: block
        C12: do j = 1, n
           D12: block
                hits = 'd' // hits
           end block D12
           hits = 'c' // hits
        end do C12
        if (i==1) exit
        hits = 'b' // hits
     end block B12
     hits = 'a' // hits
  end do A12
  print *, 12, hits
  if (hits /= 'cd  ') error stop 120

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
     if (i==1) exit A13
     hits = 'a' // hits
  end do A13
  print *, 13, hits
  if (hits /= 'bcd ') error stop 130


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
     if (i==1) exit
     hits = 'a' // hits
  end do A14
  print *, 14, hits
  if (hits /= 'bcd ') error stop 140

end program BExit
