! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an array dummy argument (assumed-shape)
!*                               Direct Access
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
      contains
         procedure, pass :: getC
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
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   subroutine myWrite1(unit, stat, msg, recn, a, b )
      class(base), intent(in) :: a(:)
      class(base), intent(in), optional :: b(:,:)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: recn
      character(*), intent(inout) :: msg

      if (.not. present(b) ) then
         write(unit, iostat=stat, iomsg=msg, rec=recn) a
      else
      	 write(unit, iostat=stat, iomsg=msg, rec=recn) a,b
      end if
   end subroutine

   subroutine myWrite2(unit, stat, msg, recn, a )
      class(base), intent(in) :: a(2,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: recn
      character(*), intent(inout) :: msg

      write (unit, iostat=stat, iomsg=msg, rec=recn) a(1:2,1)

   end subroutine

end module

program dummyArg002
   use m1

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type(base), allocatable  :: b3(:,:)
   type(base) :: b4(2:4)                       !<= explicit shape array
   integer :: stat
   character(200) :: msg
   character(8)  :: c1, c3, c5, c6, c7, c8
   character(20) :: c2
   character(28) :: c4

   ! allocation of variables
   allocate ( b1(2), source = (/ base('abc'), base('def') /) )
   allocate ( b2(1,3), source = reshape ( source = (/ base('ABC'), base('DEF') , base('GHI') /), shape=(/1,3/)) )
   allocate ( b3(2,2), source = reshape ( source = (/ b1, base('ghi'), base('jkl') /), shape = (/2,2/) ) )
   b4 =(/ base('ABC'), base('DEF') , base('GHI') /)


   open (unit = 1, file ='dummyArg002.data', form='unformatted', access='direct', recl=50)

   ! unformatted I/O operations

   call myWrite1 (1, stat, msg, 8, b1 )                !<- write 'abcZdefZ' to file
   call myWrite1 (1, stat, msg, 7, b1, b2 )            !<- write 'abcZdefZABCZDEFZGHIZ' to file
   call myWrite1 (1, stat, msg, 6, b4(2:4:2))          !<- write 'ABCZGHIZ' to file
   call myWrite1 (1, stat, msg, 5, b4, b3 )            !<- write 'ABCZDEFZGHIZabcZdefZghiZjklZ' to file

   call myWrite2 (1, stat, msg, 4, b1 )                !<- write 'abcZdefZ' to file
   call myWrite2 (1, stat, msg, 3, b2(1,1:3:2) )       !<- write 'ABCZGHIZ' to file
   call myWrite2 (1, stat, msg, 2, b3(1,1:2) )         !<- write 'abcZghiZ' to file
   call myWrite2 (1, stat, msg, 1, b4((/2,4,3/)) )     !<- write 'ABCZGHIZ' to file

   read (1, iostat=stat, iomsg=msg, rec=8)              c1
   read (1, iostat=stat, iomsg=msg, rec=7)              c2
   read (1, iostat=stat, iomsg=msg, rec=6)              c3
   read (1, iostat=stat, iomsg=msg, rec=5)              c4

   read (1, iostat=stat, iomsg=msg, rec=4)              c5
   read (1, iostat=stat, iomsg=msg, rec=3)              c6
   read (1, iostat=stat, iomsg=msg, rec=2)              c7
   read (1, iostat=stat, iomsg=msg, rec=1)              c8

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZ' )                 error stop 1_4
   if ( c2 /= 'abcZdefZABCZDEFZGHIZ' )     error stop 2_4
   if ( c3 /= 'ABCZGHIZ' )                 error stop 3_4
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

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
