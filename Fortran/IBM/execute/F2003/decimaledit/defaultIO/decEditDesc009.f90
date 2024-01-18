!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/26/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the character array used in format
!                               specification.
!                               In FMT=, if the char-expr is an array, it is
!                               treated as if all of the elements of the array
!                               were specified in array element order and were
!                               concactenated.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program decEditDesc009
     character(:), pointer :: format(:)

     type base
        integer id
        character(20) :: format = ''
     end type

     type (base) :: b1 (10)

     allocate (character(10) :: format(5))

     b1%id = (/(i, i=1, 10)/)

     b1%format = (/character(15) :: '(', 'dc,', 'g12.3,', 'g12.3,', 'g5', &
            '', ')', '', '', ''/)

     format(1) = '('
     format(3) = ',dc'
     format(2) = 'f12.3'
     format(4) = ',es12.3'
     format(5) = ',/)'

     write (*, '(dc, "test 1")')
     write (*, format) (1.2, .321e8)
     write (*, format((/1,2,4,3,5/))) (sin(i*1.0), i=1, 10)

     write (*, '(dc, sp, a)') "test 2"

     write (*, b1%format) (cmplx(sin(i*1.0), 1.0), mod(i,2) == 0, i=1, 10)
end
