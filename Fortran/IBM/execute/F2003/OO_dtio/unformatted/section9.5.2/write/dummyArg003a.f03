! OO_dtio/unformatted/section9.5.2/write/dummyArg003a.f, xlftest.OO_dtio, tstdev, 1.1
! Extract Date/Time: 05/02/17 14:27:58
! Checkin Date/Time: 04/12/03 18:06:11
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an unlimited polymorphic array
!*                               Stream Access
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

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   subroutine myWrite1(unit, stat, msg, pos, a, b )
      class(*), intent(in) :: a(:)
      class(*), intent(in) :: b(:,:)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: pos
      character(*), intent(inout) :: msg

      select type (a)
         class is (base)
            select type (b)
               class is (base)
                  write(unit, iostat=stat, iomsg=msg, pos=pos) a,b
            end select
      end select

   end subroutine

   subroutine myWrite2(unit, stat, msg, pos, a )
      class(*), intent(in) :: a(2,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: pos
      character(*), intent(inout) :: msg

      select type (a)
         class is (base)
            write(unit, iostat=stat, iomsg=msg, pos= pos) a(1:2,1)
      end select

   end subroutine

end module

program dummyArg003a
   use m1

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type(base), allocatable  :: b3(:,:)
   type(base) :: b4(2:4)                       !<= explicit shape array
   integer :: stat
   character(200) :: msg
   character(8)  :: c5, c6, c7, c8
   character(20) :: c2
   character(28) :: c4

   ! allocation of variables
   allocate ( b1(2), source = (/ base('abc'), base('def') /) )
   allocate ( b2(1,3), source = reshape ( source = (/ base('ABC'), base('DEF') , base('GHI') /), shape=(/1,3/)) )
   allocate ( b3(2,2), source = reshape ( source = (/ b1, base('ghi'), base('jkl') /), shape = (/2,2/) ) )
   b4 =(/ base('ABC'), base('DEF') , base('GHI') /)

   open (unit = 1, file ='dummyArg003a.data', form='unformatted', access='stream')

   ! unformatted I/O operations

   call myWrite1 (1, stat, msg, 60, b1, b2 )
   call myWrite1 (1, stat, msg, 1, b4, b3 )

   call myWrite2 (1, stat, msg, 200, b1 )
   call myWrite2 (1, stat, msg, 180, b2(1,1:3:2) )
   call myWrite2 (1, stat, msg, 160, b3(1,1:2) )
   call myWrite2 (1, stat, msg, 140, b4((/2,4,3/)) )


   read (1, iostat=stat, iomsg=msg, pos=60  )              c2
   read (1, iostat=stat, iomsg=msg, pos=1   )              c4

   read (1, iostat=stat, iomsg=msg, pos=200 )              c5
   read (1, iostat=stat, iomsg=msg, pos=180 )              c6
   read (1, iostat=stat, iomsg=msg, pos=160 )              c7
   read (1, iostat=stat, iomsg=msg, pos=140 )              c8

   ! check if the values are set correctly

   if ( c2 /= 'abcZdefZABCZDEFZGHIZ' )             error stop 2_4
   if ( c4 /= 'ABCZDEFZGHIZabcZdefZghiZjklZ' )     error stop 4_4

   if ( c5 /= 'abcZdefZ' )                 error stop 5_4
   if ( c6 /= 'ABCZGHIZ' )                 error stop 6_4
   if ( c7 /= 'abcZghiZ' )                 error stop 7_4
   if ( c8 /= 'ABCZGHIZ' )                 error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine
