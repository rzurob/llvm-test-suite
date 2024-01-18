!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : extNaNInfIO006.f
!*
!*  DATE                       : July 18, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=oldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing DT I/O with IEEE NaN and Inf formats supported by extension.
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

          if ( iotype .eq. "LISTDIRECTED" .or. vlist(1) .eq. 0 ) then
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

          if ( iotype .eq. "LISTDIRECTED" .or. vlist(1) .eq. 0 ) then
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

program extNaNInfIO006

      use m
      implicit none

      integer, parameter :: in = 11, out = 12

      type(Point) :: pt1, pt2, pt3

      open(in,  file='extNaNInfIO006.dat', action='read')
      open(out, file='extNaNInfIO006.out', action='write')

      call pt1%reset
      call pt2%reset
      call pt3%reset

      read(in,*) pt1
      read(in,*) pt2
      read(in,*) pt3

      ! check for the signs of NaN values
      if ( .not. equiv_is_positive(pt1%y) ) error stop 1_4
      if ( .not. equiv_is_positive(imag(pt1%cx_value)) ) error stop 2_4
      if ( .not. equiv_is_negative(pt2%x) ) error stop 3_4
      if ( .not. equiv_is_negative(pt2%y) ) error stop 4_4
      if ( .not. equiv_is_positive(real(pt2%cx_value)) ) error stop 5_4
      if ( .not. equiv_is_negative(pt3%x) ) error stop 6_4
      if ( .not. equiv_is_negative(pt3%y) ) error stop 7_4
      if ( .not. equiv_is_positive(imag(pt3%cx_value)) ) error stop 8_4

      write(out,"(dt'FORMATTED'(3,7,8))") pt1
      write(out,"(dt'FORMATTED'(3,7,8))") pt2
      write(out,"(dt'FORMATTED'(2,5,5))") pt3

      call pt1%reset
      call pt2%reset
      call pt3%reset

      read(in,"(2(dt'FORMATTED'(3,4,8)))") pt1, pt2
      read(in,"(dt'FORMATTED'(3,5,4))") pt3

      ! check for the signs of NaN values
      if ( .not. equiv_is_positive(pt1%x) ) error stop 9_4
      if ( .not. equiv_is_positive(pt1%y) ) error stop 10_4
      if ( .not. equiv_is_negative(real(pt1%cx_value)) ) error stop 11_4
      if ( .not. equiv_is_negative(pt2%x) ) error stop 12_4
      if ( .not. equiv_is_positive(pt2%y) ) error stop 13_4
      if ( .not. equiv_is_negative(imag(pt2%cx_value)) ) error stop 14_4
      if ( .not. equiv_is_negative(pt3%x) ) error stop 15_4
      if ( .not. equiv_is_positive(pt3%y) ) error stop 16_4
      if ( .not. equiv_is_negative(imag(pt3%cx_value)) ) error stop 17_4

      write(out, *, delim='quote', decimal='comma') pt1, pt2
      write(out,"(dt'FORMATTED'(3,5,4))") pt1
      write(out, *, sign='plus', round='up') pt3

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
