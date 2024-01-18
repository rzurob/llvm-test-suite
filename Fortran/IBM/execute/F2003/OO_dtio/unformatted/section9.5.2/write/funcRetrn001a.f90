!#######################################################################
! SCCS ID Information                                                   
! %W%, %I%                                                              
! Extract Date/Time: %D% %T%                                            
! Checkin Date/Time: %E% %U%                                            
!#######################################################################
! ********************************************************************* 
! %START                                                                
! %MAIN: YES                                                            
! %PRECMD: rm -f *.mod                                                  
! %COMPOPTS: -qfree=f90                                                 
! %GROUP: funcRetrn001a.f                                               
! %VERIFY:                                                              
! %STDIN:                                                               
! %STDOUT:                                                              
! %EXECARGS:                                                            
! %POSTCMD:                                                             
! %END                                                                  
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab 
!*                             :  
!*                                
!*  PRIMARY FUNCTIONS TESTED   :  
!*                             :  
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  DRIVER STANZA              : xlf95
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
   type base                                                      
      character(3) :: c = ''                                      
      contains                                                    
         procedure, pass :: getC                                  
         procedure, pass :: setC                                  
   end type                                                       
                                                                  
contains                                                          
   elemental function getC (a)                                              
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
                                                                  
program funcRetrn001a                                              
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
                                                                  
   ! interface for external polymorphic function return function   
                                                                  
   interface                                                      
      function foo ()                                             
         import base                                              
         class(base), pointer, dimension(:,:) :: foo                              
      end function                                                
   end interface                                                  
                                                                  
   interface                                                      
      function boo ()                                             
         import base                                              
         type(base), dimension(2) :: boo                              
      end function                                                
   end interface                                               
                                                                  
   ! declaration of variables                                     
   integer :: stat                                                
   character(200) :: msg                                          
   character(6)   :: c1
   character(16)  :: c2
   character(24)  :: c3
   character(20)  :: c4
   class(base), allocatable :: b1(:)                                
                                                                  
   ! allocation of variables                                      

   allocate(b1(2), source = (/ base('ibm'), base('grt') /) )
                                                                                                                     
   open (unit = 1, file ='funcRetrn001a.data', form='unformatted', access='sequential')                               
                                                                                                                     
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
                                                                                                                        
   if ( c1 /= 'ibmgrt' )                      error stop 1_4                                                             
   if ( c2 /= 'fooZabcZdefZghiZ' )            error stop 2_4                                                             
   if ( c3 /= 'fooZabcZdefZghiZbooZabcZ' )    error stop 3_4                                                             
   if ( c4 /= 'booZabcZbarZabcZdefZ' )        error stop 4_4                                                             
                                                                                                                     
   ! close the file appropriately                                                                                    
                                                                                                                     
   close ( 1, status ='delete' )                                                                                     
                                                                                                                     
contains                                                                                                             
   function bar()                                                                                                    
      class(base), pointer, dimension(:) :: bar                                                                                    
      allocate(bar(3) , source = (/ base('bar'), base('abc'), base('def') /))                                                                           
   end function                                                                                                      
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

function foo()                                                                                                       
use m1                                                                                                               
   class(base), pointer, dimension(:,:) :: foo                                                                                       
   allocate(foo(2,2), source = reshape (source=(/base('foo'),base('abc'),base('def'),base('ghi') /), shape=(/2,2/))  )
end function                                                                                                         

function boo()
use m1
   type(base) :: boo(2)
   boo = (/ base('boo'), base('abc') /)
end function                                                                                                         