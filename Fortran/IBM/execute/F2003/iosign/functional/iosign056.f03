!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN and
!*  INQUIRE stmts for files with direct access.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign056

    complex :: q = (1.0,1.0)
    complex(4) :: q4 = (1.0,1.0)
    complex(8) :: q8 = (1.0d0,1.0d0)
    complex(16) :: q16 = (1.0q0,1.0q0)
    character(20) :: ns

    ! Test 1

    open (1,file="iosign055.1",form='formatted',status='replace', &
          access='direct',recl=40,sign='processor_defined')

    inquire(1,sign=ns)

    close(1)

    if ( ns .ne. 'PROCESSOR_DEFINED' ) then
      print *, "Error for sign inquiry!!"
      error stop 1
    end if

    ! Test 2

    open (1,file="iosign055.1",form='formatted',status='old',&
          access='direct',recl=40,sign='suppress')

    inquire(1,sign=ns)

    close(1)

    if ( ns .ne. 'SUPPRESS' ) then
      print *, "Error for sign inquiry!!"
      error stop 2
    end if

    ! Test 3

    open (1,file="iosign055.1",form='formatted',status='old',&
          access='direct',recl=40,sign='plus')

    inquire(1,sign=ns)

    close(1)

    if ( ns .ne. 'PLUS' ) then
      print *, "Error for sign inquiry!!"
      error stop 3
    end if

end program iosign056

