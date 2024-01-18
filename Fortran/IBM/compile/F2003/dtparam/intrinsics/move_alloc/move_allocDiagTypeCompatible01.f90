!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 29 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. TO: TYPE SHALL BE COMPATIBLE WITH FROM AND HAVE SAME RANK
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type A(k1,l1)
        integer,kind :: k1=4
        integer,len  :: l1=2
    end type
    type,extends(A)  :: C(k2)
         integer,kind :: k2=8
    end type
    type B(k1,l1)
        integer,kind :: k1=4
        integer,len  :: l1=2
    end type
end module

program move_allocDiagTypeCompatible01

  use m
  implicit none


  type(A(4,2)),allocatable   :: a1
  type(C(4,2,3)),allocatable :: c1

  type(A(4,2)),allocatable   :: a2(:)
  type(A(4,2)),allocatable   :: a3(:,:)

  class(A(4,2)),allocatable  :: a4
  class(C(4,2)),allocatable  :: c2

  class(A(4,2)),allocatable  :: a5(:)
  class(A(4,2)),allocatable  :: a6(:,:)

  type(B(4,2)),allocatable   :: b1

  call move_alloc(from=a1,to=c1)
  call move_alloc(from=c1,to=a1)
  call move_alloc(from=a4,to=c2) ! c2 is not compatible with a4
  call move_alloc(from=c2,to=a4) ! a4 is compatible with c2,no error msg this line

  call move_alloc(from=a1,to=b1)
  call move_alloc(from=b1,to=a1)

  call move_alloc(from=a2,to=a3)
  call move_alloc(from=a3,to=a2)
  call move_alloc(from=a5,to=a6)
  call move_alloc(from=a6,to=a5)

end program

