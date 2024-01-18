!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : cannot cycle a block
!*  ADAPTED FROM               : BCycle
!*
!*  DESCRIPTION
!*
!*  CYCLE statements must be in a DO loop and "jump" to the named DO construct when
!*  a name appears in the statement, and otherwise to the nearest containing DO loop.
!*  Unlike the EXIT statement, we can *only* cycle to a DO construct, and not to a
!*  BLOCK.  This case tests the generation of diagnostic error messages as we attempt
!*  to cycle to a BLOCK.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BNoCycled

  implicit none
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
                if (i==1) cycle D1 ! wrong
                hits = 'd' // hits
           end block D1
           hits = 'c' // hits
        end do C1
        hits = 'b' // hits
     end block B1
     hits = 'a' // hits
  end do A1
  print *, 1, hits

  hits = ''
  A3: do i = 1, m
     B3: block
        C3: do j = 1, n
           D3: block
                if (i==1) cycle B3 ! wrong
                hits = 'd' // hits
           end block D3
           hits = 'c' // hits
        end do C3
        hits = 'b' // hits
     end block B3
     hits = 'a' // hits
  end do A3
  print *, 3, hits

  hits = ''
  A7: do i = 1, m
     B7: block
        C7: do j = 1, n
           D7: block
                hits = 'd' // hits
           end block D7
           if (i==1) cycle B7 ! wrong
           hits = 'c' // hits
        end do C7
        hits = 'b' // hits
     end block B7
     hits = 'a' // hits
  end do A7
  print *, 7, hits

  hits = ''
  A10: do i = 1, m
     B10: block
        C10: do j = 1, n
           D10: block
                hits = 'd' // hits
           end block D10
           hits = 'c' // hits
        end do C10
        if (i==1) cycle B10 ! wrong
        hits = 'b' // hits
     end block B10
     hits = 'a' // hits
  end do A10
  print *, 10, hits

  B15: block
    if (m == 1) cycle B15 ! wrong
  end block B15

  block
    if (m == 1) cycle ! wrong
  end block

end program BNoCycled
