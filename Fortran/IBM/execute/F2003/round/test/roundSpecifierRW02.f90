!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier= in READ/WRITE statement.
!*                             
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

  program roundSpecifierRW02 

    use m
    character*17 rMode(2)
    real r

    allocate(c2, source=roundTwo(c1))

    open(12, file="tstIn.dat")

    read(12, fmt='(f8.5)', round=roundOne('down')) r
    
    open(13, file="roundSpecifierRW02.out")

    write(13, fmt='(f8.4)', round=c2) r

    contains

      character(:) function roundOne ( c )
         character(4) c
         allocatable :: roundOne 

         allocate (roundOne, source = c )
      end function

      character(:) function roundTwo( c )
         character(:), allocatable :: c
         allocatable :: roundTwo 

         allocate ( c, source = "up" )

         allocate ( roundTwo, source = c )

      end function

  end program roundSpecifierRW02 
