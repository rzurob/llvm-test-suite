!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO012.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing Input / Output of IEEE NaN and Inf with user defined DT I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m

        implicit none
        
        type :: Point
          real(kind=4)    :: x, y
          complex(kind=4) :: cx_value
          character(3)    :: name
          contains
            procedure, pass :: reset
            procedure       :: childi
            procedure       :: childo
            generic         :: write(formatted) => childo
            generic         :: read(formatted)  => childi
        end type
      
      contains
        subroutine reset(arg)
          class(Point) :: arg
          arg%x = -1.0
          arg%y = -1.0
          arg%cx_value = (-1.0, -1.0)
          arg%name = 'xxx'
        end subroutine

        subroutine childi(dtv, unit, iotype, vlist, iostat, iomsg)
          ! arguments:
          class(Point), intent(inout) :: dtv
          integer, intent(in)         :: unit
          character(*), intent(in)    :: iotype
          integer, intent(in)         :: vlist(:)
          integer, intent(out)        :: iostat
          character(*), intent(inout) :: iomsg

          ! local variables:
          character(64) :: myfmt

          if ( iotype .eq. "LISTDIRECTED" ) then
             read(unit, *) dtv%name, dtv%x, dtv%y, dtv%cx_value
          else
             write(myfmt, '(a2, i1, a4, i2, a6, i2, a3)')              &
     &            '(a', vlist(1), ', 2f', vlist(2), '.2, 2f',          &
     &            vlist(3), '.2)'

             ! myfmt : '(aX, 2f X.2, 2f X.2) '
             read(unit, myfmt) dtv%name, dtv%x, dtv%y, dtv%cx_value
          endif

          
        end subroutine

        subroutine childo(dtv, unit, iotype, vlist, iostat, iomsg)
          ! arguments:
          class(Point), intent(in)    :: dtv
          integer, intent(in)         :: unit
          character(*), intent(in)    :: iotype
          integer, intent(in)         :: vlist(:)
          integer, intent(out)        :: iostat
          character(*), intent(inout) :: iomsg

          ! local variables:
          character(64) :: myfmt

          if ( iotype .eq. "LISTDIRECTED" ) then
             write(unit, *) dtv%name, " = {", dtv%x, ',', dtv%y, ',',  &
     &                      dtv%cx_value, "}"
          else
             write(myfmt, '(a2, i1, a4, i2, a6, i2, a3)')              &
     &            '(a', vlist(1), ', 2f', vlist(2), '.2, 2f',          &
     &            vlist(3), '.2)'

             ! myfmt : '(aX, 2f X.2, 2f X.2) '
             write(unit, myfmt) dtv%name, dtv%x, dtv%y, dtv%cx_value

          endif
          
        end subroutine

      end module
      
program miscNaNInfIO012

      use m
      implicit none

      integer, parameter :: in = 11, out = 12

      type(Point) :: pt1, pt2
      
      open(in,  file='miscNaNInfIO012.dat', action='read')
      open(out, file='miscNaNInfIO012.out', action='write')

      call pt1%reset
      call pt2%reset
      
      read(in,*) pt1
      read(in,*) pt2

      ! check for the signs of NaN values
      if ( .not. equiv_is_negative(pt1%y) ) then
         error stop 1_4
      end if
      if ( .not. equiv_is_positive( real(pt1%cx_value) ) ) then
         error stop 2_4
      end if
      if ( .not. equiv_is_positive( imag(pt1%cx_value) ) ) then
         error stop 3_4
      end if
      if ( .not. equiv_is_positive(pt2%x) ) then
         error stop 4_4
      end if
      if ( .not. equiv_is_negative( imag(pt2%cx_value) ) ) then
         error stop 5_4
      end if      
      
      write(out,"(dt'FORMATTED'(3,7,8))") pt1
      write(out,"(dt'FORMATTED'(3,7,8))") pt2
      
      call pt1%reset
      call pt2%reset

      read(in,"(2(dt'FORMATTED'(3,9,8)))") pt1, pt2

      ! check for the signs of NaN values
      if ( .not. equiv_is_negative(pt1%y) ) then
         error stop 6_4
      end if
      if ( .not. equiv_is_negative( imag(pt1%cx_value) ) ) then
         error stop 7_4
      end if
      if ( .not. equiv_is_positive(pt2%x) ) then
         error stop 8_4
      end if
      if ( .not. equiv_is_negative(pt2%y) ) then
         error stop 9_4
      end if


      write(out, *) pt1, pt2
      write(out,"(dt'FORMATTED'(3,5,4))") pt1
      write(out, *) pt2

      close(in)
      close(out)
      
      contains

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is negative
      logical function equiv_is_negative(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq
         
         equivalence(tmp_val, val_eq)
         
         tmp_val = val
         
         if ( val_eq .ge. 0 ) then
            equiv_is_negative = .false.
         else
            equiv_is_negative = .true.
         end if

      end function

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is positive
      logical function equiv_is_positive(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq
         
         equivalence(tmp_val, val_eq)
         
         tmp_val = val

         if ( val_eq .le. 0 ) then
            equiv_is_positive = .false.
         else
            equiv_is_positive = .true.
         end if

      end function

end program
