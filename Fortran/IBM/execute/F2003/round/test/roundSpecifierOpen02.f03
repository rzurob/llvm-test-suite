!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier= in OPEN statement.
!*
!*  DESCRIPTION                :
!*                            scalar character with deferred length
!*                            as character expression. Also test function
!*                            return as character expression in round
!*                            specifier.
!* ===================================================================

  module m
    character(:), allocatable :: c1, c2
  end module

  program roundSpecifierOpen02

    use m
    character*17 rMode(2)

    allocate(c2, source=roundTwo(c1))

    open(12, file="tstRound12", round=roundOne('compatible'))

    inquire(12, round = rMode(1))

    if(rMode(1) .ne. 'COMPATIBLE') error stop 1_4

    open(13, file="tstRound13", round=c2)

    inquire(file="tstRound13", round = rMode(2))

    if(rMode(2) .ne. 'PROCESSOR_DEFINED') error stop 2_4

    contains

      character(:) function roundOne ( c )
         character(10) c
         allocatable :: roundOne

         allocate (roundOne, source = c )
      end function

      character(:) function roundTwo( c )
         character(:), allocatable :: c
         allocatable :: roundTwo

         allocate ( c, source = "processor_defined" )

         allocate ( roundTwo, source = c )

      end function

  end program roundSpecifierOpen02
