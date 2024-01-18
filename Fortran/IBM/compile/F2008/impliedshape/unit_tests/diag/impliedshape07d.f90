!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              assumed-size vs. implied-shape
!*
!234567890123456789012345678901234567890123456789012345678901234567890


   contains
   subroutine sub1(asize1,asize2)
     character(*) :: asize1(*)
     character(*) :: asize2(3,*)

     character(*), parameter :: ishape1(*) = ['abcd','efgh']
     character(*), parameter :: ishape2(*,*) = reshape(['abcd','efgh'],[1,2])

     print *, asize1, asize2
     print *, ishape1, ishape2
   end subroutine
   end

