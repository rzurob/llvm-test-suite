!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence02f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence 
!*                              in intrisic procedures with optional
!*                              dummy args
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer     :: pu, pa
 integer, target      :: t
 integer, allocatable :: au, aa 

 integer, parameter :: ip(2,2) = 1
 integer, parameter :: ip2(1:8,10:40) = 1

 nullify(pu)
 t = 2
 pa => t
 allocate(aa, source=2)

 ! Print the SIZE of dim 2
 print *, size(ip,t),  size(ip,2)
 print *, size(ip,aa), size(ip,2)
 print *, size(ip,au), size(ip)
 print *, size(ip,pa), size(ip,2)
 print *, size(ip,pu), size(ip)
 ! Print ubound of dim 2, or all dimensions if not present
 print *, ubound(ip2,t),  ubound(ip2,2)
 print *, ubound(ip2,aa), ubound(ip2,2)
 print *, ubound(ip2,au), ubound(ip2)
 print *, ubound(ip2,pa), ubound(ip2,2)
 print *, ubound(ip2,pu), ubound(ip2)
 end
