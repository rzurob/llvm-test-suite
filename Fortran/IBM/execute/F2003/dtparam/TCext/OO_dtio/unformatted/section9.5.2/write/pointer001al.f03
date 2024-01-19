! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2:
!*                               if input is a pointer, data are transferred from file to
!*                               the associated target.  If an output is a pointer, data shall
!*                               transfer from target to file. (WRITE, array pointer)
!*                               Sequential Access
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
   type base (lbase_1) ! lbase_1=2
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(2) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(2), intent(in) :: char
      a%c = char
   end subroutine
end module


program pointer001al
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(:)), pointer :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), allocatable, target :: b3(:), b5(:) ! tcx: (:)
   type(base(:)), allocatable, target :: b4(:,:), b6(:,:) ! tcx: (:)
   integer :: stat
   character(200) :: msg
   character(12)   :: c1
   character(24)  :: c2
   character(6)   :: c3
   character(18)  :: c4

   ! allocation of variables
   allocate ( b3(4),   source = (/ base(2)('ab'), base(2)('cd'), base(2)('ef'), base(2)('gh') /)) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b4(2,2), source = reshape( source = (/ base(2)('ij'), base(2)('kl'), base(2)('mn'), base(2)('op') /), shape=(/2,2/) ) ) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b5(4),   source = (/ base(2)('AB'), base(2)('CD'), base(2)('EF'), base(2)('GH') /)) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b6(2,2), source = reshape( source = (/ base(2)('IJ'), base(2)('KL'), base(2)('MN'), base(2)('OP') /), shape=(/2,2/) ) ) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)

   b1 => b3
   b2 => b4

   open (unit = 1, file ='pointer001al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1         !<- shall write the content of b3 to file
      if (stat /= 0 ) error stop 101_4
   write (1, iostat=stat, iomsg=msg )             b1, b2     !<- shall write the content of b3, b4 to file
      if (stat /= 0 ) error stop 2_4

   ! change pointer targets

   b1 => b5(1:4:2)                   !<- points to b5(1) and b5(3) 'ABEF'
   b2 => b6(2:1:-1,2:1:-1)           !<- points to b6 in reversed order 'OPMNKLIJ'

   write (1, iostat=stat, iomsg=msg )             b1         !<- shall write the content of b5 to file
      if (stat /= 0 ) error stop 3_4
   write (1, iostat=stat, iomsg=msg )             b1, b2     !<- shall write the content of b5, b6 to file
      if (stat /= 0 ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
      if (stat /= 0 ) error stop 5_4
   read (1, iostat=stat, iomsg=msg )              c2
      if (stat /= 0 ) error stop 6_4
   read (1, iostat=stat, iomsg=msg )              c3
      if (stat /= 0 ) error stop 7_4
   read (1, iostat=stat, iomsg=msg )              c4
      if (stat /= 0 ) error stop 8_4

   ! check if the values are set correctly

   if ( c1 /= "abZcdZefZghZ" )                  error stop 9_4
   if ( c2 /= "abZcdZefZghZijZklZmnZopZ" )      error stop 10_4
   if ( c3 /= "ABZEFZ" )                        error stop 11_4
   if ( c4 /= "ABZEFZOPZMNZKLZIJZ" )            error stop 12_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (2) / declare with (*) - 23 changes
