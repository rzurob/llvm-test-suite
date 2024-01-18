!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355440.f
!*
!*  DATE                       : August 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 355440
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: A(l1)
     integer,len  :: l1
   end type

   type :: C(l3)
      integer,len     :: l3
      type(A(l3)),pointer  :: a2=>null()
   end type

end module

program d355440
  use m
  implicit none

  type(A(:)),pointer :: t2
  character(len=7) :: ch1="xlftest"
  type(C(:)),allocatable :: c1

  print *, 3,len(ch1(1:3))
  allocate(c1,source=C(3)())
  allocate(t2,source=A(3)())
  c1%a2=>t2

  call check(c1)
  deallocate(t2)
  allocate(t2,source=A(len(ch1(1:3)))())
  call check(c1)
  contains

    subroutine check(dt)
       type(C(*)) :: dt
           print *,dt%a2%l1
    end subroutine

end

