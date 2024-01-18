! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of class(*)
!*                               dynamic type is character(*)
!*                               FROM is dummy arg of module procedure
!*                               TO is module variable
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module m
          class(*), allocatable :: l2(:,:)
          class(*), allocatable :: l1(:,:)

          contains
              subroutine sub(arg)
                  class(*), allocatable :: arg(:,:)

                  allocate ( character(4) :: arg(2, 2) )

                  select type ( arg)
                      type is (character(*))
                          arg  = reshape ((/ 'ibm ', 'a bc', ' c++', 'plix' /), (/2,2/)  )
                  end select

                  call move_alloc(arg, l2)

              end subroutine
      end module

      use m

      call sub(l1)

      if ( allocated(l1 ) ) stop 11
      if ( .not. allocated(l2) ) stop 13

      select type (l2)
            type is (character(*) )
                if ( l2(1,1) /= 'ibm ' ) stop 21
                if ( l2(2,1) /= 'a bc ' ) stop 23
                if ( l2(1,2) /= ' c++' ) stop 25
                if ( l2(2,2) /= 'plix' ) stop 27
      end select

      end
