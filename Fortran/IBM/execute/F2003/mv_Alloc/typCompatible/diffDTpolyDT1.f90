! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of a parent poly DT
!*                               FROM is of a child non-poly DT, component of DT
!*                               integer pointer as component of parent DT
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A
          integer, pointer :: ip => null()
      end type

      type , extends(A) :: B
          class(A), allocatable :: p
      end type

      type(A), allocatable ::  a1
      class(A), allocatable :: a2

      type(B) ::  b1
      type(B), allocatable :: b2

      integer , target :: id, id1, id2, id3

      id = 11
      allocate(a1, source = A(id))

      id1 = 21
      allocate(a2, source = A(id1))

      id2 = 31
      allocate(b2, source = b(id2,a2))

      id3 = 41
      b1 = b(id3, a1)

      call move_alloc(b2, b1%p)

      if ( allocated(b2 ) ) error stop 11
      if ( .not. allocated(b1%p) ) error stop 20

      select type (x => b1%p)
          type is (B)
              if ( x%ip /= 31 ) error stop 21
              select type ( y => x%p)
                  type is (A)
                      if ( y%ip /= 21 ) error stop 23
                  class default
                      stop 25
              end select
          class default
              stop 31
      end select
      end
