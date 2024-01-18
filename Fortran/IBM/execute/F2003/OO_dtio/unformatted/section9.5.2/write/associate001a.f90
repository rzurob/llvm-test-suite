! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate name (associated with an array)
!*                               Sequential Access
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
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(3), intent(in) :: char
      a%c = char
   end subroutine
end module


program associate001a
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), pointer :: b1(:)
   class(base), allocatable, target :: b2 (:,:)
   integer :: stat
   character(200) :: msg
   character(6) :: c1, c2
   character(8) :: c3, c4

   ! allocation of variables

   allocate ( b1(4), source = (/ base('abc'), base('xxx'), base('def'),base('xxx') /)  )  !<- 1 dimensional array
   allocate ( b2(2,2), source = reshape (source=(/base('ghi'),base('jkl'),base('mno'),base('pqr') /), shape=(/2,2/)) )   !<- 2 dimensional array

   open (unit = 1, file ='associate001a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )   b1(1:3:2)%c         !<- shall write 'abcdef' to file
   write (1, iostat=stat, iomsg=msg )   b2(1,1:2)%c         !<- shall write 'ghimno' to file

   associate ( b11 => b1, b12 => b2(1,1:2) )
      write (1, iostat=stat, iomsg=msg )   b11(1:3:2)  !<- shall write 'abcZdefZ' to file
      write (1, iostat=stat, iomsg=msg )   b12(1:2)    !<- shall write 'ghiZmnoZ' to file
   end associate

   rewind 1

   read (1, iostat=stat, iomsg=msg )       c1          !<- shall read 'abcdef' from file
   read (1, iostat=stat, iomsg=msg )       c2          !<- shall read 'ghimno' from file
   read (1, iostat=stat, iomsg=msg )       c3          !<- shall read 'abcZdefZ' from file
   read (1, iostat=stat, iomsg=msg )       c4          !<- shall read 'ghiZmnoZ' from file

   ! check if the values are set correctly

   if ( c1 /= 'abcdef' )          error stop 1_4
   if ( c2 /= 'ghimno' )          error stop 2_4
   if ( c3 /= 'abcZdefZ' )        error stop 3_4
   if ( c4 /= 'ghiZmnoZ' )        error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
