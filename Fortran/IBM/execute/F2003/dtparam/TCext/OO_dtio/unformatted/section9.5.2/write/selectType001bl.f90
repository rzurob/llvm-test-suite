! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectType001bl
!*
!*  PROGRAMMER                 : David Forster (derived from selectType001b by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type
   
   type, extends(base) :: child
      character(lbase_1) :: cc = ''   
   end type

end module

program selectType001bl
   use m1   

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         use m1
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables
   class(base(:)), pointer      :: b1(:) ! tcx: (:)
   class(child(:)), allocatable :: b2(:,:) ! tcx: (:)
   class(base(:)), allocatable  :: b3(:) ! tcx: (:)
   integer :: stat
   character(200) :: msg = ''
   character(14) :: c1
   character(28) :: c2
   character(8) :: c3
   
   ! allocation of variables
   
   allocate ( b1(3), source = (/ child(3)('abc', 'def'), child(3)('ghi', 'jkl'), child(3)('mno', 'pqr') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape( source = (/ child(3)('ABC','DEF'), child(3)('GHI','JKL'), & ! tcx: (3) ! tcx: (3)
                                child(3)('MNO','PQR'), child(3)('STU','VWX') /), shape = (/2,2/)) ) ! tcx: (3) ! tcx: (3)
   allocate ( b3(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /)) ! tcx: (3) ! tcx: (3) ! tcx: (3)
      
   open (unit = 1, file ='selectType001bl.data', form='unformatted', access='stream' )
   
   ! unformatted I/O operations
   
   select type (b11 => b1(1:3:2) )
      class is (base(*)) ! tcx: (*)
         write (1, iostat=stat, iomsg=msg, pos=37 )    b11
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 101_4 
      class default
         error stop 2_4
   end select
   
   select type (b12 => b2)
      class is (child(*)) ! tcx: (*)
         write (1, iostat=stat, iomsg=msg, pos=9 )    b12
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 3_4          
      class default
         error stop 4_4
   end select

   select type (b13 => b3((/3,1/)) )
      class is (base(*)) ! tcx: (*)
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
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    
    if ( iostat /= 0 ) error stop 13_4
    
    select type (dtv)
       type is (child(*)) ! tcx: (*)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select
    
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
    
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 10 changes
