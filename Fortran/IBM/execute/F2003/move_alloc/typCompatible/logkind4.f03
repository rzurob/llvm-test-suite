! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of logical, TO of unlimit poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type base
      end type

      type, extends(base) :: child
      end type

      class(*), allocatable ::  a1,a2
      type(base) b1
      type(child) c1

      logical, allocatable :: l1(:,:)
      class(*), allocatable :: o(:,:)

      allocate ( l1(2,2), source= reshape ( (/ extends_type_of(a1,a2), &
                     extends_type_of(a2, b1), extends_type_of(c1, b1), &
                     extends_type_of(b1,c1) /), (/2,2/) ) )

       call move_alloc(l1, o)

       if ( .not. allocated(o) ) error stop 21
       if ( allocated(l1) ) error stop 23

       print *, shape(o)

       select type (o)
              type is (logical)
                  print *, o
              class default
                  stop 21
       end select
       end
