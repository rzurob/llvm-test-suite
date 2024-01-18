! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : blank003akl
!*
!*  PROGRAMMER                 : David Forster (derived from blank003a by Robert Ma)
!*  DATE                       : 2007-07-21 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.7.6: BN, BZ Editing
!*                                        When BZ mode has no effect on character editing
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

   type :: base (lb) ! lb=3
      integer, len :: lb
      character(lb), allocatable   :: c(:)
   end type
      
   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

end module

program blank003akl
   use m1   

   ! declaration of variables

   class(base(:)), allocatable  :: f1 ! tcx: (:)
   type(base(:)), pointer       :: f2(:) ! tcx: (:)
   type(base(:)) , allocatable  :: f3 ! tcx: (:)
   class(base(:)), pointer      :: f4(:) ! tcx: (:)
      
   integer :: stat
   character(200) :: msg
   
   procedure(character(20)) :: getFormat   
   open ( 1, file = 'blank003akl.1', form='formatted', access='sequential', blank='zero' )    !<- initially set blank mode to null
   
   ! allocation of variables
  
   allocate (base(3):: f1,f2(2)) ! tcx: base(3)
   allocate ( f1%c(2), source = (/ 'IBM', 'ibm' /) ) 
   allocate ( f2(1)%c(2), source = (/ 'IBM', 'ibm' /) ) 
   allocate ( f2(2)%c(5), source = (/ 'IBM', 'ibm', 'IBM', 'ibm' , 'iBm' /) ) 
   
   write (1, *)   "                                                               "
   write (1, *)   "                                                               "

   rewind 1
   
   ! formatted I/O operations
   
   read (1, '(DT)', iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4
   
   read (1, '(2DT)', iostat=stat, iomsg=msg)                f2   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4
   
   print *, f1%c,"!"
   print *, f2(1)%c,"!"
   print *, f2(2)%c,"!"
   
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   procedure(character(20)) :: getFormat
   
   read ( unit, iostat = iostat, fmt = getFormat(size(dtv%c,1)) )   dtv%c  
   
   iomsg = 'dtioread'
   
end subroutine

character(20) function getFormat(i)
   integer, intent(in) :: i
   write (getFormat,"(A,I2,A)") "(BZ,",i,"(A,1X) )"
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 6 changes
