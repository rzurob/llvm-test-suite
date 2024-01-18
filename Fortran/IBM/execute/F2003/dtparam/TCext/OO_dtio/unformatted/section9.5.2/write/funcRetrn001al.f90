! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn001al
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn001a by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be a function return
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''                                      
      contains                                                    
         procedure, pass :: getC                                  
         procedure, pass :: setC                                  
   end type                                                       
                                                                  
contains                                                          
   elemental function getC (a)                                              
      class(base(*)), intent(in) :: a                                 ! tcx: (*)
      character(3) :: getC                                        
      getC = a%c                                                  
   end function                                                   
                                                                  
   subroutine setC (a, char)                                      
      class(base(*)), intent(inout) :: a                              ! tcx: (*)
      character(3), intent(in) :: char                            
      a%c = char                                                  
   end subroutine                                                 
end module                                                        
                                                                  
program funcRetrn001al                                              
   use m1                                                         
                                                                  
   interface write(unformatted)                                   
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)      
         import base                                              
         class(base(*)), intent(in) :: dtv                            ! tcx: (*)
         integer,  intent(in) :: unit                             
         integer,  intent(out) :: iostat                          
         character(*),  intent(inout) :: iomsg                    
      end subroutine                                              
   end interface                                                  
                                                                  
   ! interface for external polymorphic function return function   
                                                                  
   interface                                                      
      function foo ()                                             
         import base                                              
         class(base(:)), pointer, dimension(:,:) :: foo                               ! tcx: (:)
      end function                                                
   end interface                                                  
                                                                  
   interface                                                      
      function boo ()                                             
         import base                                              
         type(base(3)), dimension(2) :: boo                               ! tcx: (3)
      end function                                                
   end interface                                               
                                                                  
   ! declaration of variables                                     
   integer :: stat                                                
   character(200) :: msg                                          
   character(6)   :: c1
   character(16)  :: c2
   character(24)  :: c3
   character(20)  :: c4
   class(base(:)), allocatable :: b1(:)                                 ! tcx: (:)
                                                                  
   ! allocation of variables                                      

   allocate(b1(2), source = (/ base(3)('ibm'), base(3)('grt') /) ) ! tcx: (3) ! tcx: (3)
                                                                                                                     
   open (unit = 1, file ='funcRetrn001al.data', form='unformatted', access='sequential')                               
                                                                                                                     
   ! unformatted I/O operations                                                                                      
                                                                                                                     
   write (1, iostat=stat, iomsg=msg )             b1%getC()         !<- write 'ibmgrt' to file (type bound)             
   write (1, iostat=stat, iomsg=msg )             foo()             !<- external function with polymorphic return    
   write (1, iostat=stat, iomsg=msg )             foo(), boo()      !<- external function with non-polymorphic return
   write (1, iostat=stat, iomsg=msg )             boo(), bar()      !<- internal function with polymorphic return    
                                                                                                                     
   rewind 1                                                                                                          
                                                                                                                     
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4 
                                                                                                                     
   ! check if the values are set correctly                                                                           
                                                                                                                        
   if ( c1 /= 'ibmgrt' )                      error stop 101_4                                                             
   if ( c2 /= 'fooZabcZdefZghiZ' )            error stop 2_4                                                             
   if ( c3 /= 'fooZabcZdefZghiZbooZabcZ' )    error stop 3_4                                                             
   if ( c4 /= 'booZabcZbarZabcZdefZ' )        error stop 4_4                                                             
                                                                                                                     
   ! close the file appropriately                                                                                    
                                                                                                                     
   close ( 1, status ='delete' )                                                                                     
                                                                                                                     
contains                                                                                                             
   function bar()                                                                                                    
      class(base(:)), pointer, dimension(:) :: bar                                                                                     ! tcx: (:)
      allocate(bar(3) , source = (/ base(3)('bar'), base(3)('abc'), base(3)('def') /))                                                                            ! tcx: (3) ! tcx: (3) ! tcx: (3)
   end function                                                                                                      
end program                                                                                                          
                                                                                                                     
subroutine writeUnformatted (dtv, unit, iostat, iomsg)                                                               
use m1                                                                                                               
    class(base(*)), intent(in) :: dtv                                                                                    ! tcx: (*)
    integer, intent(in) :: unit                                                                                      
    integer, intent(out) :: iostat                                                                                   
    character(*), intent(inout) :: iomsg                                                                                

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine                                                                                                       

function foo()                                                                                                       
use m1                                                                                                               
   class(base(:)), pointer, dimension(:,:) :: foo                                                                                        ! tcx: (:)
   allocate(foo(2,2), source = reshape (source=(/base(3)('foo'),base(3)('abc'),base(3)('def'),base(3)('ghi') /), shape=(/2,2/))  ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
end function                                                                                                         

function boo()
use m1
   type(base(3)) :: boo(2) ! tcx: (3)
   boo = (/ base(3)('boo'), base(3)('abc') /) ! tcx: (3) ! tcx: (3)
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 21 changes
