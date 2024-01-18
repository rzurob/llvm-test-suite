! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedLen101f.f
!*
!* PROGRAMMER                   : Maryam Moghadas
!* DATE                         : June  25, 2014
!* ORIGIN                       : AIX Complier Development
!*                              
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily) 
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an Scalar, all possible actual 
!*                                      arguments are tested 
!*                                - Call to BIND(C) procedure from different scopes:
!*                                      main program, internal/external procedure
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main  
  interface
    subroutine sub_1(arg2) bind(c)
      character(*) :: arg2
    end subroutine
    subroutine sub(arg) bind(c)
     character(:), allocatable :: arg
    end subroutine
  end interface
 
  character(7) :: c1 
 
  character(:), pointer :: c2

  character(:), allocatable :: c3

  character(:), pointer :: c4
  character(:), allocatable, target :: t4

  character(:), pointer :: c5
  character(7), target :: t5

  ! constant expression actual argument 
  character(7), parameter :: c6 = "IBM2014" 
  
  ! expression actual argument 
  character(7) :: c7

  c1 = "IBM2014" 

  allocate(character(7) :: c2)
  c2 = "IBM2014"

  allocate(character(7) :: c3)
  c3 = "IBM2014"

  allocate(character(7) :: t4)
  t4 = "IBM2014"
  c4 => t4

  t5 = "IBM2014"
  c5 => t5

  c7 = "IBM" // "2014"


!-calling sub_1 from another internal subroutine
  call sub_int(c1)
  call sub_int(c2)
  call sub_int(c3)
  call sub_int(c4)
  call sub_int(c5)
  call sub_int(c6)     ! Asti ICE - defect 77053
  call sub_int(c7)
  call sub_1(c6 // "")

!-calling sub_1 directly from main program
  call sub_1(c1) 
  call sub_1(c2)
  call sub_1(c3)
  call sub_1(c4)
  call sub_1(c5)
  call sub_1(c6)       ! Asti ICE - defect 77053
  call sub_1(c7)
  call sub_1(c6 // "")  

!-calling sub_1 from another external subprogram  
! actual arg to sub_1 will be a CFI descriptor 
  call sub(c3)

  contains
    subroutine sub_int(arg_int)
      character(*) :: arg_int
      call sub_1(arg_int)
    end subroutine sub_int
end program

subroutine sub(arg) bind(c)
  character(:), allocatable :: arg
  interface
   subroutine sub_1(arg1) bind(c)
    character(*) :: arg1
   end subroutine
  end interface
  call sub_1(arg)
end subroutine



