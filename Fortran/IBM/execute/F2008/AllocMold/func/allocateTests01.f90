!* ==================================================================
!* XL Fortran Test Case                          IBM INTERNAL USE ONL
!* ==================================================================
!*
!* TEST CASE TITLE            : allocateTests01.f
!*
!* PROGRAMMER                 : Izhak Jakov
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!* SECONDARY FUNCTIONS TESTED :
!*
!*
!* DRIVER STANZA              : xlf2008
!* REQUIRED COMPILER OPTIONS  :
!*
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!*
!* DESCRIPTION                : Modified unit test to verify the
!*                              the print statements
!*
!*
!* Defect 112510
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
MODULE M

  TYPE ONE
    INTEGER*4 :: id
  END TYPE

  TYPE  :: Zero
    PRIVATE
  END TYPE 

  TYPE, EXTENDS(Zero)  :: Base
    INTEGER :: BaseId = 1
  END TYPE

  TYPE, EXTENDS(Base) :: Child
    INTEGER  :: ChildId = 2
  END TYPE

END MODULE


program main
  use m

  character            :: nl   ! New Line Character
  integer, allocatable :: i,j,k
  integer(kind=4)      :: q
  complex(8)           :: g(3,3)
  type (Child), target, allocatable :: w,x,y,z

  complex(16), allocatable :: a(:,:,:), b
  complex(8),  allocatable :: c(:,:), d(:)

  integer, pointer:: p1,p2,p3,p4,p5,p6,p7

  type (one), pointer :: b1, b2
  class(one), pointer :: b3, b4

  nl = new_line('')

  allocate(b1, b3, source=one(id=90), stat=q)
  if (q.ne.0) error stop(10)
  print *, "B1 = ", b1%id, nl, "B3 = ", b3%id 
  
  allocate(w,x, source=Child(BaseID=-1, ChildID=-2), stat=q)
  if (q.ne.0) error stop(20)
  allocate(y,z, mold=Child(BaseID=-1, ChildID=-2), stat=q)
  if (q.ne.0) error stop(30)
  print *, "W = ", w, nl, "X = ", x
  print *, "Y = ", y, nl, "Z = ", z
 
  allocate(a(2,2,2), b, source=(3.0_16,4.0_16), stat=q)
  if (q.ne.0) error stop(40)
  allocate(c, mold=g, stat=q)
  if (q.ne.0) error stop(45)
  allocate(d, mold=(reshape(g,(/1/))), stat=q)
  if (q.ne.0) error stop(50)
  print *, "A =", a, nl, "B =", b
  print *, "C =", c, nl, "D =", d
  if ( lbound(c,1) .NE.   1 ) error stop(901)
  if ( ubound(c,1) .NE.   3 ) error stop(902)
  if ( lbound(c,2) .NE.   1 ) error stop(903)
  if ( ubound(c,2) .NE.   3 ) error stop(904)
  if ( lbound(d,1) .NE.   1 ) error stop(905)
  if ( ubound(d,1) .NE.   1 ) error stop(906)
  
  allocate(p4,p7, mold=22, stat=q)
  if (q.ne.0) error stop(55)
  print *, "P4 =", p4, nl, "P7 =", p7
  
  p4=-22
  allocate(p3)
  p3=p4
  print *, "P3 =", p3        
  allocate(p1,p2, source=p3, stat=q)
  if (q.ne.0) error stop(60)
  print *, "P1 =", p1, nl, "P2 =", p2
 
  print *, "P7 =", p7 
  p7=p4
  print *, "P7 =", p7
  allocate(p5,p6, mold=p7, stat=q)
  if (q.ne.0) error stop(70)
  print *, "P5 =", p5, nl, "P6 =", p6
  
  allocate (b2, b4, source=b1, stat=q)
  if (q.ne.0) error stop(80)
  print *, "B2 =", b2%id, nl, "B4 =", b4%id
  
  allocate (b2, b4, mold=b3, stat=q)
  if (q.ne.0) error stop(90)
  print *, "B2 =", b2%id, nl, "B4 =", b4%id
  
  k = 59

  allocate (i,j, source=k, stat=q)
  if (q.ne.0) error stop(100)
  print *, "I =", i, nl, "J =", j
  
  deallocate(i,j)
  allocate (i, mold=-3, stat=q)
  if (q.ne.0) error stop(110)
  allocate (j,source=7,  stat=q)
  if (q.ne.0) error stop(115)
  if (.not. allocated(i)) error stop(118)
  print *, "J =", j
  deallocate(i,j)

  allocate (i,j, mold=k, stat=q)
  if (q.ne.0) error stop(120)
  if (.not. allocated(i)) error stop(125)
  if (.not. allocated(j)) error stop(128)
          
  deallocate(i,j)
  allocate (i,j,mold=2, stat=q)
  if (q.ne.0) error stop(130)
  if (.not. allocated(i)) error stop(135)
  if (.not. allocated(j)) error stop(138)
  deallocate(i,j)

end program main
