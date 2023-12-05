program aoc5_part2
  implicit none
  integer, parameter :: bigint = selected_int_kind(32)
  character(209) :: line
  integer :: stat
  integer(kind=bigint) :: seeds(20)
  integer(kind=bigint) :: map_seed_soil(3, 50), &
             map_soil_fert(3, 50), &
             map_fert_water(3, 50), &
             map_water_light(3, 50), &
             map_light_temp(3, 50), &
             map_temp_hum(3, 50), &
             map_hum_loc(3, 50)

  map_seed_soil = 0
  map_soil_fert = 0
  map_fert_water = 0
  map_water_light = 0
  map_light_temp = 0
  map_temp_hum = 0
  map_hum_loc = 0

  open(10, file='input.txt', status='old', iostat=stat)

  block
    integer :: i, ws, word_count, line_count, line_len
    logical :: word_started, num, first_line
    character(40) :: word, section, tmp
    section = '                                        '
    line_count = 0
    first_line = .true.
    do while (.true.)
      if (first_line) then
        read(10, "(A7,I10,I8,I10,I9,I9,I10,I11,I10,I11,I10,I10,I10,I11,I11,I11,I10,I11,I10,I10,I10)") tmp, seeds
        first_line = .false.
      else
        read(10, "(A)", end=99) line
      end if
      line_count = line_count + 1

      word_started = .true.
      ws = 1
      word_count = 0

      if (is_number(line(1:1))) then
        line_len = len_trim(line)+1
      else
        line_len = len_trim(line)
      end if

      do i = 1, line_len
        if (is_space(line(i:i))) then
          word_count = word_count+1
          word = line(ws:i-1)
          if (.not. num) then
            section = word
            word_count = 0
            line_count = 0
          else
            select case (trim(section))
            case ('seeds:')
!              read (word, '(I12)') seeds(word_count)
            case ('seed-to-soil')
              read (word, '(I12)') map_seed_soil(word_count, line_count)
            case('soil-to-fertilizer')
              read (word, '(I12)') map_soil_fert(word_count, line_count)
            case('fertilizer-to-water')
              read (word, '(I12)') map_fert_water(word_count, line_count)
            case('water-to-light')
              read (word, '(I12)') map_water_light(word_count, line_count)
            case('light-to-temperature')
              read (word, '(I12)') map_light_temp(word_count, line_count)
            case('temperature-to-humidity')
              read (word, '(I12)') map_temp_hum(word_count, line_count)
            case('humidity-to-location')
              read (word, '(I12)') map_hum_loc(word_count, line_count)
            end select
          end if
          ws = i+1
        else if (is_number(line(i:i))) then
          num = .true.
        else
          num = .false.
        end if
      end do
    end do
  end block

  99 continue

  block
    integer :: i_range
    integer(kind=bigint) :: num, seed
    integer(kind=bigint) :: minlocs(10)
    integer(kind=bigint) :: loc, minseedloc, i_seed, n_seeds, i_cum


    do i_range = 1, 10

      n_seeds = seeds(2*i_range)

      write(*, "(A7,X,I10,X,A5,X,I2,A1,I2)") "Writing", n_seeds, "seeds", i_range,"/",10

      i_cum=0
      do i_seed = 1, n_seeds
        i_cum=i_cum+1

        block
          real :: percentage_cum, percentage

          percentage_cum = (100.0*i_cum)/n_seeds
          percentage = (100.0*i_seed)/n_seeds
          
          if (percentage_cum > 5) then
            i_cum = 0
            write(*, "(A11,X,F3.0,X,A1)") "Seeds ready", percentage, "%"
          end if
        end block
        seed = generate_seed_number(seeds, i_range, i_seed)
        loc = map_numbers(map_hum_loc, &
                    map_numbers(map_temp_hum, &
                    map_numbers(map_light_temp, &
                    map_numbers(map_water_light, &
                    map_numbers(map_fert_water, &
                    map_numbers(map_soil_fert, &
                    map_numbers(map_seed_soil, seed))&
                  )))))
        if (i_seed==1) then
          minseedloc=loc
        else if (loc<minseedloc) then
          minseedloc=loc
        end if
        
      end do
    end do
      
    print *, minseedloc

  end block

  contains

    subroutine print_map(map)
      integer(kind=bigint) :: map(:,:)
      integer :: i

      do i = 1, size(map, dim=2)
        write(*, '(3(I10,X))') map(1:3,i)
      end do
    end subroutine

    pure logical function is_number(ch) result(o)
      character(1), intent(in) :: ch

      o = ch .ge. '0' .and. ch .le. '9'
    end function

    pure logical function is_space(ch) result(o)
      character(1), intent(in) :: ch

      o = ch == ' '
    end function

    integer(kind=bigint) function map_numbers(map, num) result (numout)
      integer(kind=bigint), intent(in) :: map(:, :)
      integer(kind=bigint), intent(in) :: num
      integer(kind=bigint) :: l, u, pos
      integer :: i

      numout = num
      do i = 1, size(map, dim=2)
        l = map(2, i)
        u = map(2, i) + map(3, i) - 1
        if (l .le. num .and. u .ge. num) then
          pos = num-l
          numout = map(1, i) + pos

!          write(*, "(4(A1,X,I10,X))") "l", l, "u", u, "num", num, "pos", pos
!          print *, map(1, i)
        end if
      end do
      
    end function

    function generate_seed_number(seed_table, i_range, i_seed) result (seed)
      integer(kind=bigint), intent(in) :: seed_table(20)
      integer(kind=bigint), intent(in) :: i_seed
      integer(kind=bigint) :: seed
      integer(kind=bigint) :: l, rang, n_seeds
      integer :: i_range, i, j

      rang = seed_table(2*i_range)
      l = seed_table(2*i_range-1)

      seed = l+i_seed-1

    end function

end program aoc5_part2
