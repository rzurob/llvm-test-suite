! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of a derived-type
!*                               use implict to specify default type
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


     program main

          type t
             sequence
             integer i
          end type

          implicit type(t)  (a)

          allocatable a1

          call sub()

          contains
               subroutine sub
                  implicit type(t)  (n)

                  allocatable n1

                  allocate(a1, source = t(10))
                  call move_alloc(a1, n1)
                  if ( .not. allocated(n1) ) stop 21
                  if ( allocated(a1) ) stop 31
                  if ( n1%i /= 10 ) stop 41
               end subroutine

     end program

