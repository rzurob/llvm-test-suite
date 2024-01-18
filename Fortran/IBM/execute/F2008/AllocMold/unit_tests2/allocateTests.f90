  MODULE M

    type ONE
      integer*4 :: id
    end type

    TYPE  :: Zero
      private
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE


PROGRAM MAIN
  USE M
  INTEGER,allocatable :: i,j,k
  TYPE (Child),   TARGET, ALLOCATABLE :: W,X,Y,Z

  complex(8), allocatable :: a(:,:,:), b
  complex(8), allocatable :: c(:,:,:), d

  integer, pointer:: p1,p2,p3
  integer, pointer:: p5,p6,p7

  type (ONE), pointer :: b1, b2
  class(one), pointer :: b3, b4

  ALLOCATE(W,X, SOURCE=Child(BaseID=-1, ChildID=-2))
  ALLOCATE(Y,Z, MOLD=Child(BaseID=-1, ChildID=-2))
 
  allocate(a(2,2,2), b, source=(3.0_8,4.0_8))
  allocate(c(2,2,2),d, mold=(3.0_8,4.0_8))

  allocate(p3)
  allocate(p1,p2,source=p3)
  allocate(p5,p6,mold=p7)

  allocate(b3)
  allocate (b2, b4, source=b3)
  allocate (b2, b4, mold=b1)

  k = -7
  allocate (i,j,source=k)
  deallocate(i,j)
  
  allocate (i,j,source=2)
  deallocate(i,j)
  
  allocate (i,j,mold=k)
  deallocate(i,j)
  
  allocate (i,j,mold=2)
  deallocate(i,j)

END PROGRAM MAIN
