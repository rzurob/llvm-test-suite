!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType001b.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate name (from select type construct)
!*                                 with arrays being the selector
!*                               Stream Access
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

end module

program selectType001b
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         use m1
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), pointer      :: b1(:)
   class(child), allocatable :: b2(:,:)
   class(base), allocatable  :: b3(:)
   integer :: stat
   character(200) :: msg = ''
   character(14) :: c1
   character(28) :: c2
   character(8) :: c3

   ! allocation of variables

   allocate ( b1(3), source = (/ child('abc', 'def'), child('ghi', 'jkl'), child('mno', 'pqr') /) )
   allocate ( b2(2,2), source = reshape( source = (/ child('ABC','DEF'), child('GHI','JKL'), &
                                child('MNO','PQR'), child('STU','VWX') /), shape = (/2,2/)) )
   allocate ( b3(3), source = (/ base('abc'), base('def'), base('ghi') /))

   open (unit = 1, file ='selectType001b.data', form='unformatted', access='stream' )

   ! unformatted I/O operations

   select type (b11 => b1(1:3:2) )
      class is (base)
         write (1, iostat=stat, iomsg=msg, pos=37 )    b11
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type (b12 => b2)
      class is (child)
         write (1, iostat=stat, iomsg=msg, pos=9 )    b12
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 3_4
      class default
         error stop 4_4
   end select

   select type (b13 => b3((/3,1/)) )
      class is (base)
         write (1, iostat=stat, iomsg=msg, pos=1 )    b13
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 5_4
      class default
         error stop 6_4
   end select

   read (1, iostat=stat, iomsg=msg, pos=37 )       c1
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 7_4
   read (1, iostat=stat, iomsg=msg, pos=9 )        c2
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 8_4
   read (1, iostat=stat, iomsg=msg, pos=1 )        c3
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 9_4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefZmnopqrZ' )                      error stop 10_4
   if ( c2 /= 'ABCDEFZGHIJKLZMNOPQRZSTUVWXZ' )        error stop 11_4
   if ( c3 /= 'ghiZabcZ' )                            error stop 12_4

   !close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 13_4

    select type (dtv)
       type is (child)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
