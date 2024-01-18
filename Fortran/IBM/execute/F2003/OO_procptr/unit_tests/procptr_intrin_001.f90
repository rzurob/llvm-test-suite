       !
       ! Use a specific intrinsic procedure as iface-name
       !
       !procedure(iabs), pointer :: pp1
       ! ( 345917 )
       procedure(len), pointer :: pp1
       pp1 => len
       if ( pp1("")  .ne. 0 ) stop 10
       if ( pp1(" ") .ne. 1 ) stop 11
       end
