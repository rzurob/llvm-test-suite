!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  defect 361546
!234567890123456789012345678901234567890123456789012345678901234567890
program d361546
  type A(l)
    integer,len :: l
  end type

  type(A(:)),allocatable :: a,b

  allocate(A(3) :: a)

  call alloc(a,b)

  if(b%l /= 3)     stop

  contains

    subroutine alloc(a,b)
        type(A(*)),intent(in) :: a
        type(A(:)),allocatable :: b

        b=a
    end subroutine

end program
