       !
       ! Use a generic/specific intrinsic procedure as iface-name
       ! c1212 & c1215 limits that only len() as an intrinsic can be used in
       ! this situation. So make it diagnostic situation
       !
       procedure(abs), pointer :: pp1
       procedure(iabs), pointer :: pp2
       !real iii, jjj
       !pp1 => abs
       !jjj = -19.0
       !iii = pp1(jjj)
       !if (iii .ne. 19.0) stop 10
       end
