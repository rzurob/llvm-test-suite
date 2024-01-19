!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier=
!*
!*  DESCRIPTION                : test round with inquire and data
!*                               transfer statement inside subprogram.
!*
!* ===================================================================

  module m
    character(10), parameter :: fileName = 'tstRound'

    contains

    subroutine openFile (unit, fileName, roundMode)
        integer, intent(in) :: unit
        character(*), intent(in) :: fileName, roundMode

        open (unit, file=fileName, round=roundMode, form='formatted')
    end subroutine

    subroutine writeFile (unit, fileName, roundMode)
        integer, intent(in) :: unit
        character(*), intent(in) :: fileName, roundMode
        real x, y

        write (unit, *, round=roundMode) x, y
    end subroutine

    subroutine inquireRoundModeByFile (fileName, roundMode)
        character(*), intent(in) :: fileName
        character(*), intent(out) :: roundMode

        inquire (file=fileName, round=roundMode)
    end subroutine

    subroutine inquireRoundModeByUnit (unit, roundMode)
        integer, intent(in) :: unit
        character(*), intent(out) :: roundMode

        inquire (unit, round=roundMode)
    end subroutine

  end module

  program roundSpecifierInquire02
     use m
     character(:), allocatable :: c(:)

     allocate (character(18) :: c(5))

     call inquireRoundModeByFile (fileName, c(1))

     call openFile (10, fileName//'   ', 'zero')

     call inquireRoundModeByUnit(10, c(2))

     call openFile (10, fileName//'   ', 'compatible')

     call inquireRoundModeByUnit(10, c(3))

     call inquireRoundModeByFile (fileName, c(4)(:))

     call writeFile (10, fileName//'   ', 'up ')

     call inquireRoundModeByFile (fileName, c(5))

     if(c(1) .ne. 'UNDEFINED') error stop 1_4
     if(c(2) .ne. 'ZERO') error stop 2_4
     if(c(3) .ne. 'COMPATIBLE') error stop 3_4
     if(c(4) .ne. 'COMPATIBLE') error stop 4_4
     if(c(5) .ne. 'COMPATIBLE') error stop 5_4

  end program roundSpecifierInquire02
