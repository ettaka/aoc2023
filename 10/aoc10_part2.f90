program aoc10_part1
  implicit none

!  integer, parameter :: max_i = 9, max_j = 11
  integer, parameter :: max_i = 140, max_j = 140
  character(1) :: map(max_i,max_j), route(max_i, max_j)
  integer :: stat
  integer :: start(2), curpos(2), oldpos(2), oldoldpos(2)
  integer :: step_counts(max_i, max_j), direction(max_i, max_j)

  step_counts = 0
  direction = 0
  block
    integer :: i, j
    character(max_j) :: line
!    open(10, file='input_small2.txt', status='old', iostat=stat)
    open(10, file='input.txt', status='old', iostat=stat)
    do i=1,max_i
        read(10, '(A)') line
        do j=1,max_j
          map(i,j) = line(j:j)
          if (map(i,j) == 'S') then
            start(1) = i
            start(2) = j
          end if
        end do
    end do
    close(10)
  end block

  call print_map(map)

  curpos = start
  block
    integer :: step, diff(2), secondpos(2)

    route(:,:) = '.'
    call print_pos(map, curpos)
    do step = 1, max_i*max_j
      if (step == 2) secondpos = curpos
      if (step > 1) oldoldpos = oldpos
      oldpos = curpos
      call march_one(map, curpos, route, (step==2), (step==-2))
      if (step > 1) then
        diff = curpos-oldoldpos
        if (diff(1) == 2 .and. diff(2)==0) direction(oldpos(1), oldpos(2)) = 1
        if (diff(1) == 1 .and. diff(2)==1) direction(oldpos(1), oldpos(2)) = 2
        if (diff(1) == 0 .and. diff(2)==2) direction(oldpos(1), oldpos(2)) = 3
        if (diff(1) == -1 .and. diff(2)==1) direction(oldpos(1), oldpos(2)) = 4
        if (diff(1) == -2 .and. diff(2)==0) direction(oldpos(1), oldpos(2)) = 5
        if (diff(1) == -1 .and. diff(2)==-1) direction(oldpos(1), oldpos(2)) = 6
        if (diff(1) == 0 .and. diff(2)==-2) direction(oldpos(1), oldpos(2)) = 7
        if (diff(1) == 1 .and. diff(2)==-1) direction(oldpos(1), oldpos(2)) = 8
      end if

      call print_pos(map, curpos)
      step_counts(curpos(1), curpos(2)) = step
      if (map(curpos(1), curpos(2)) == 'S') then
        print *, "Back to start!"
        exit
      end if
      route(curpos(1), curpos(2)) = map(curpos(1),curpos(2))
    end do

    diff = secondpos-oldpos
    if (diff(1) == 2 .and. diff(2)==0) direction(start(1), start(2)) = 1
    if (diff(1) == 1 .and. diff(2)==1) direction(start(1), start(2)) = 2
    if (diff(1) == 0 .and. diff(2)==2) direction(start(1), start(2)) = 3
    if (diff(1) == -1 .and. diff(2)==1) direction(start(1), start(2)) = 4
    if (diff(1) == -2 .and. diff(2)==0) direction(start(1), start(2)) = 5
    if (diff(1) == -1 .and. diff(2)==-1) direction(start(1), start(2)) = 6
    if (diff(1) == 0 .and. diff(2)==-2) direction(start(1), start(2)) = 7
    if (diff(1) == 1 .and. diff(2)==-1) direction(start(1), start(2)) = 8

    call print_map(route)
    print *, "step_count", step
    print *, "half_way", step/2
  end block

  block
    integer :: i, j, area, n_vertical, dir
    character(1) :: ch
    logical :: counting_started, vertical_pipe, any_pipe, inside_loop

    area = 0
    do i=1,max_i
      n_vertical = 0
      inside_loop = .false.
      do j=1,max_j

        dir = direction(i,j)
        if (dir == 4 .or. dir == 5 .or. dir == 6) inside_loop = .false.
        if (dir == 8 .or. dir == 1 .or. dir == 2) inside_loop = .true.

        if (inside_loop) then
          if (direction(i, j) == 0) then
            area = area+1
            route(i,j) = 'I'
          end if
        end if

      end do
    end do
    call print_map(route)
    print *, "area", area
    call print_map_int(direction)
  end block
    
  contains

    subroutine print_pos(map, curpos)
      character(1), intent(in) :: map(:,:)
      integer :: curpos(2)
      write (*, "(A19,X,2I4,X,A)") "position: ", curpos, map(curpos(1), curpos(2))
    end subroutine

    subroutine print_map(map)
      character(1), intent(in) :: map(:,:)
      integer :: i, j

      do i = 1, size(map, dim=1)
        print *, map(i,:)
      end do
    end subroutine

    subroutine print_map_int(map)
      integer, intent(in) :: map(:,:)
      integer :: i, j

      do i = 1, size(map, dim=1)
        write(*, "(140I1)") map(i,:)
      end do
    end subroutine

    subroutine march_one(map, curpos, route, dont_go_east, dont_go_west)
      character(1), intent(in) :: map(:,:), route(:,:)
      logical, intent(in) :: dont_go_east, dont_go_west
      integer :: curpos(2)
      character(1) :: curchar, ch
      logical :: visited_east, visited_west, visited_south, visited_north
      logical :: can_go_east, can_go_west, can_go_south, can_go_north
      ! | is a vertical pipe connecting north and south.
      ! - is a horizontal pipe connecting east and west.
      ! L is a 90-degree bend connecting north and east.
      ! J is a 90-degree bend connecting north and west.
      ! 7 is a 90-degree bend connecting south and west.
      ! F is a 90-degree bend connecting south and east.
      ! . is ground; there is no pipe in this tile.
      ! S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

      curchar = map(curpos(1), curpos(2))

      visited_east = route(curpos(1), curpos(2)+1) .ne. '.'
      visited_north = route(curpos(1)-1, curpos(2)) .ne. '.'
      visited_west = route(curpos(1), curpos(2)-1) .ne. '.'
      visited_south = route(curpos(1)+1, curpos(2)) .ne. '.'

      ch = map(curpos(1), curpos(2)+1)
      can_go_east = (ch == '-' &
              .or. ch == 'J' &
              .or. ch == '7' &
              .or. ch == 'S') .and. .not. dont_go_east
      ch = map(curpos(1)-1, curpos(2))
      can_go_north = (ch == '|' &
              .or. ch == '7' &
              .or. ch == 'F' &
              .or. ch == 'S')
      ch = map(curpos(1), curpos(2)-1)
      can_go_west = (ch == '-' &
              .or. ch == 'L' &
              .or. ch == 'F' &
              .or. ch == 'S') .and. .not. dont_go_west
      ch = map(curpos(1)+1, curpos(2))
      can_go_south = (ch == '|' &
              .or. ch == 'J' &
              .or. ch == 'L' &
              .or. ch == 'S')

      if (can_go_east .and. .not. visited_east &
                 .and. (curchar == '-' & ! east
                   .or. curchar == 'L' &
                   .or. curchar == 'F' &
                   .or. curchar == 'S' )) then
        if (curpos(2)+1 <= max_j) then
              curpos(2) = curpos(2)+1
        end if
      else if (can_go_north .and. .not. visited_north &
                 .and. (curchar == '|' & ! north
                   .or. curchar == 'L' &
                   .or. curchar == 'J' &
                   .or. curchar == 'S' )) then
        if (curpos(1)-1 >= 1) then
          curpos(1) = curpos(1)-1
        end if
      else if (can_go_west .and. .not. visited_west &
                 .and. (curchar == '-' & ! west
                   .or. curchar == 'J' &
                   .or. curchar == '7' &
                   .or. curchar == 'S' )) then
        if (curpos(2)-1 >= 1) then
          curpos(2) = curpos(2)-1
        end if
      else if (can_go_south .and. .not. visited_south &
                 .and. (curchar == '|' & ! south
                   .or. curchar == '7' &
                   .or. curchar == 'F' &
                   .or. curchar == 'S' )) then
        if (curpos(1)+1 <= max_i) then
            curpos(1) = curpos(1)+1
        end if
      end if
    end subroutine

end program aoc10_part1
