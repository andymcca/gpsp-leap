/* gameplaySP
 *
 * Copyright (C) 2006 Exophase <exophase@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "common.h"
#include <sys/poll.h>
#define MAX_DEVNODES	18
static int pox_evdev = -1;


// Special thanks to psp298 for the analog->dpad code!

void trigger_key(u32 key)
{
  u32 p1_cnt = io_registers[REG_P1CNT];

  if((p1_cnt >> 14) & 0x01)
  {
    u32 key_intersection = (p1_cnt & key) & 0x3FF;

    if(p1_cnt >> 15)
    {
      if(key_intersection == (p1_cnt & 0x3FF))
        raise_interrupt(IRQ_KEYPAD);
    }
    else
    {
      if(key_intersection)
        raise_interrupt(IRQ_KEYPAD);
    }
  }
}

u32 key = 0;

u32 global_enable_analog = 1;
u32 analog_sensitivity_level = 4;

typedef enum
{
  BUTTON_NOT_HELD,
  BUTTON_HELD_INITIAL,
  BUTTON_HELD_REPEAT
} button_repeat_state_type;


// These define autorepeat values (in microseconds), tweak as necessary.

#define BUTTON_REPEAT_START    200000
#define BUTTON_REPEAT_CONTINUE 50000

button_repeat_state_type button_repeat_state = BUTTON_NOT_HELD;
u32 button_repeat = 0;
gui_action_type cursor_repeat = CURSOR_NONE;


#ifdef PSP_BUILD

u32 gamepad_config_map[16] =
{
  BUTTON_ID_MENU,               // Triangle
  BUTTON_ID_A,                  // Circle
  BUTTON_ID_B,                  // Cross
  BUTTON_ID_START,              // Square
  BUTTON_ID_L,                  // Ltrigger
  BUTTON_ID_R,                  // Rtrigger
  BUTTON_ID_DOWN,               // Down
  BUTTON_ID_LEFT,               // Left
  BUTTON_ID_UP,                 // Up
  BUTTON_ID_RIGHT,              // Right
  BUTTON_ID_SELECT,             // Select
  BUTTON_ID_START,              // Start
  BUTTON_ID_UP,                 // Analog up
  BUTTON_ID_DOWN,               // Analog down
  BUTTON_ID_LEFT,               // Analog left
  BUTTON_ID_RIGHT               // Analog right
};

#define PSP_ALL_BUTTON_MASK 0xFFFF

gui_action_type get_gui_input()
{
  SceCtrlData ctrl_data;
  gui_action_type new_button = CURSOR_NONE;
  u32 new_buttons;

  static u32 last_buttons = 0;
  static u64 button_repeat_timestamp;

  delay_us(25000);

  sceCtrlPeekBufferPositive(&ctrl_data, 1);
  ctrl_data.Buttons &= PSP_ALL_BUTTON_MASK;
  new_buttons = (last_buttons ^ ctrl_data.Buttons) & ctrl_data.Buttons;
  last_buttons = ctrl_data.Buttons;

  if(new_buttons & PSP_CTRL_LEFT)
    new_button = CURSOR_LEFT;

  if(new_buttons & PSP_CTRL_RIGHT)
    new_button = CURSOR_RIGHT;

  if(new_buttons & PSP_CTRL_UP)
    new_button = CURSOR_UP;

  if(new_buttons & PSP_CTRL_DOWN)
    new_button = CURSOR_DOWN;

  if(new_buttons & PSP_CTRL_START)
    new_button = CURSOR_SELECT;

  if(new_buttons & PSP_CTRL_CIRCLE)
    new_button = CURSOR_SELECT;

  if(new_buttons & PSP_CTRL_CROSS)
    new_button = CURSOR_EXIT;

  if(new_buttons & PSP_CTRL_SQUARE)
    new_button = CURSOR_BACK;

  if(new_button != CURSOR_NONE)
  {
    get_ticks_us(&button_repeat_timestamp);
    button_repeat_state = BUTTON_HELD_INITIAL;
    button_repeat = new_buttons;
    cursor_repeat = new_button;
  }
  else
  {
    if(ctrl_data.Buttons & button_repeat)
    {
      u64 new_ticks;
      get_ticks_us(&new_ticks);

      if(button_repeat_state == BUTTON_HELD_INITIAL)
      {
        if((new_ticks - button_repeat_timestamp) >
         BUTTON_REPEAT_START)
        {
          new_button = cursor_repeat;
          button_repeat_timestamp = new_ticks;
          button_repeat_state = BUTTON_HELD_REPEAT;
        }
      }

      if(button_repeat_state == BUTTON_HELD_REPEAT)
      {
        if((new_ticks - button_repeat_timestamp) >
         BUTTON_REPEAT_CONTINUE)
        {
          new_button = cursor_repeat;
          button_repeat_timestamp = new_ticks;
        }
      }
    }
  }

  return new_button;
}

#define PSP_CTRL_ANALOG_UP    (1 << 28)
#define PSP_CTRL_ANALOG_DOWN  (1 << 29)
#define PSP_CTRL_ANALOG_LEFT  (1 << 30)
#define PSP_CTRL_ANALOG_RIGHT (1 << 31)

u32 button_psp_mask_to_config[] =
{
  PSP_CTRL_TRIANGLE,
  PSP_CTRL_CIRCLE,
  PSP_CTRL_CROSS,
  PSP_CTRL_SQUARE,
  PSP_CTRL_LTRIGGER,
  PSP_CTRL_RTRIGGER,
  PSP_CTRL_DOWN,
  PSP_CTRL_LEFT,
  PSP_CTRL_UP,
  PSP_CTRL_RIGHT,
  PSP_CTRL_SELECT,
  PSP_CTRL_START,
  PSP_CTRL_ANALOG_UP,
  PSP_CTRL_ANALOG_DOWN,
  PSP_CTRL_ANALOG_LEFT,
  PSP_CTRL_ANALOG_RIGHT
};

u32 button_id_to_gba_mask[] =
{
  BUTTON_UP,
  BUTTON_DOWN,
  BUTTON_LEFT,
  BUTTON_RIGHT,
  BUTTON_A,
  BUTTON_B,
  BUTTON_L,
  BUTTON_R,
  BUTTON_START,
  BUTTON_SELECT,
  BUTTON_NONE,
  BUTTON_NONE,
  BUTTON_NONE,
  BUTTON_NONE
};

gui_action_type get_gui_input_fs_hold(u32 button_id)
{
  gui_action_type new_button = get_gui_input();
  if((last_buttons & button_psp_mask_to_config[button_id]) == 0)
    return CURSOR_BACK;

  return new_button;
}

u32 rapidfire_flag = 1;

u32 update_input()
{
  SceCtrlData ctrl_data;
  u32 buttons;
  u32 non_repeat_buttons;
  u32 button_id;
  u32 i;
  u32 new_key = 0;
  u32 analog_sensitivity = 92 - (analog_sensitivity_level * 4);
  u32 inv_analog_sensitivity = 256 - analog_sensitivity;

  sceCtrlPeekBufferPositive(&ctrl_data, 1);

  buttons = ctrl_data.Buttons;

  if(global_enable_analog)
  {
    if(ctrl_data.Lx < analog_sensitivity)
      buttons |= PSP_CTRL_ANALOG_LEFT;

    if(ctrl_data.Lx > inv_analog_sensitivity)
      buttons |= PSP_CTRL_ANALOG_RIGHT;

    if(ctrl_data.Ly < analog_sensitivity)
      buttons |= PSP_CTRL_ANALOG_UP;

    if(ctrl_data.Ly > inv_analog_sensitivity)
      buttons |= PSP_CTRL_ANALOG_DOWN;
  }

  non_repeat_buttons = (last_buttons ^ buttons) & buttons;
  last_buttons = buttons;

  for(i = 0; i < 16; i++)
  {
    if(non_repeat_buttons & button_psp_mask_to_config[i])
      button_id = gamepad_config_map[i];
    else
      button_id = BUTTON_ID_NONE;

    switch(button_id)
    {
      case BUTTON_ID_MENU:
      {
        u16 *screen_copy = copy_screen();
        u32 ret_val = menu(screen_copy);
        free(screen_copy);

        return ret_val;
      }

      case BUTTON_ID_LOADSTATE:
      {
        u8 current_savestate_filename[512];
        get_savestate_filename_noshot(savestate_slot,
         current_savestate_filename);
        load_state(current_savestate_filename);
        return 1;
      }

      case BUTTON_ID_SAVESTATE:
      {
        u8 current_savestate_filename[512];
        u16 *current_screen = copy_screen();
        get_savestate_filename_noshot(savestate_slot,
         current_savestate_filename);
        save_state(current_savestate_filename, current_screen);
        free(current_screen);
        return 0;
      }

      case BUTTON_ID_FASTFORWARD:
        print_string("FASTFORWARD", 0xFFFF, 0x0000, 0, 50);
        synchronize_flag ^= 1;
        return 0;
    }

    if(buttons & button_psp_mask_to_config[i])
    {
      button_id = gamepad_config_map[i];
      if(button_id < BUTTON_ID_MENU)
      {
        new_key |= button_id_to_gba_mask[button_id];
      }
      else

      if((button_id >= BUTTON_ID_RAPIDFIRE_A) &&
       (button_id <= BUTTON_ID_RAPIDFIRE_L))
      {
        rapidfire_flag ^= 1;
        if(rapidfire_flag)
        {
          new_key |= button_id_to_gba_mask[button_id -
           BUTTON_ID_RAPIDFIRE_A + BUTTON_ID_A];
        }
        else
        {
          new_key &= ~button_id_to_gba_mask[button_id -
           BUTTON_ID_RAPIDFIRE_A + BUTTON_ID_A];
        }
      }
    }
  }

  if((new_key | key) != key)
    trigger_key(new_key);

  key = new_key;

  io_registers[REG_P1] = (~key) & 0x3FF;

  return 0;
}

void init_input()
{
  sceCtrlSetSamplingCycle(0);
  sceCtrlSetSamplingMode(PSP_CTRL_MODE_ANALOG);
}

#endif


#if defined(GP2X_BUILD) || defined(PND_BUILD)

extern u32 fps_debug;
extern u32 gpsp_gp2x_joystick_read(void);
//nirvous
#include <linux/input.h>


gui_action_type get_gui_input()
{
  gui_action_type gui_action = CURSOR_NONE;
  //u32 buttons = gpsp_gp2x_joystick_read();
  //u32 new_buttons;
  //static u32 last_buttons = 0;
  //static u64 button_repeat_timestamp;
  delay_us(25000);
  poxInputEvent ev;
  if(poxReadInput(&ev)) {
	  if(ev.type == POX_KEYDOWN) {
		//printf("%d \n",ev.code);
		//value=ev.code;

		  switch(ev.code)
			{
			  case KEY_ENTER:
				gui_action = CURSOR_EXIT;
				break;

			  case KEY_DOWN:
				gui_action = CURSOR_DOWN;
				break;

			  case KEY_UP:
				gui_action = CURSOR_UP;
				break;

			  case KEY_LEFT:
				gui_action = CURSOR_LEFT;
				break;

			  case KEY_RIGHT:
				gui_action = CURSOR_RIGHT;
				break;

			  case KEY_A:
				gui_action = CURSOR_SELECT;
				break;

			  case KEY_B:
				gui_action = CURSOR_BACK;
				break;
			}
		}
  }
  return gui_action;
}

u32 button_id_to_gba_mask[] =
{
  BUTTON_UP,
  BUTTON_DOWN,
  BUTTON_LEFT,
  BUTTON_RIGHT,
  BUTTON_A,
  BUTTON_B,
  BUTTON_L,
  BUTTON_R,
  BUTTON_START,
  BUTTON_SELECT,
  BUTTON_NONE,
  BUTTON_NONE,
  BUTTON_NONE,
  BUTTON_NONE
};

u32 key_map(int button)
{
  switch(button)
  {
    case KEY_L:
      return BUTTON_L;

    case KEY_R:
      return BUTTON_R;

    case KEY_DOWN:
      return BUTTON_DOWN;

    case KEY_UP:
      return BUTTON_UP;

    case KEY_LEFT:
      return BUTTON_LEFT;

    case KEY_RIGHT:
      return BUTTON_RIGHT;

    case KEY_B:
      return BUTTON_B;

    case KEY_A:
      return BUTTON_A;

    case KEY_P:
      return BUTTON_START;

    case KEY_H:
      return BUTTON_SELECT;

    default:
      return BUTTON_NONE;
  }
}


u32 update_input()
{
  poxInputEvent ev;
  if(poxReadInput(&ev)) {
	  switch(ev.type) {
		case POX_KEYDOWN:
			switch (ev.code) {
				case KEY_ENTER:
// Reggie, I R teh copy from the psp update_input
      {
        u16 *screen_copy = copy_screen();
        u32 ret_val = menu(screen_copy);
        free(screen_copy);

        return ret_val;
      }
				  // nirvous
				  //this should really jump to the menu...
				case 22: //VolumeUp
				  gp2x_sound_volume(1);
				  break;
				case 32: //VolumeDown
				  gp2x_sound_volume(0);
				  break;
			        case KEY_X:
				fps_debug ^= 1;
				break;
				default:
				  key |= key_map(ev.code);
				  trigger_key(key);
				  break;
		    }
		    break;

		case POX_KEYUP:
			key &= ~(key_map(ev.code));
			break;
	   }
   }
   io_registers[REG_P1] = (~key) & 0x3FF;
   return 0;
}


void init_input()
{

}

#endif


#if defined(RPI_BUILD)

u32 key_map(SDLKey key_sym)
{
  switch(key_sym)
  {
    case SDLK_a:
      return BUTTON_L;

    case SDLK_s:
      return BUTTON_R;

    case SDLK_DOWN:
      return BUTTON_DOWN;

    case SDLK_UP:
      return BUTTON_UP;

    case SDLK_LEFT:
      return BUTTON_LEFT;

    case SDLK_RIGHT:
      return BUTTON_RIGHT;

    case SDLK_RETURN:
      return BUTTON_START;

    case SDLK_BACKSPACE:
      return BUTTON_SELECT;

    case SDLK_x:
      return BUTTON_B;

    case SDLK_z:
      return BUTTON_A;

    default:
      return BUTTON_NONE;
  }
}
#endif

#if defined(PC_BUILD)

u32 key_map(SDLKey key_sym)
{
  switch(key_sym)
  {
    case SDLK_LSHIFT:
      return BUTTON_L;

    case SDLK_x:
      return BUTTON_R;

    case SDLK_DOWN:
      return BUTTON_DOWN;

    case SDLK_UP:
      return BUTTON_UP;

    case SDLK_LEFT:
      return BUTTON_LEFT;

    case SDLK_RIGHT:
      return BUTTON_RIGHT;

    case SDLK_RETURN:
      return BUTTON_START;

    case SDLK_RSHIFT:
      return BUTTON_SELECT;

    case SDLK_LCTRL:
      return BUTTON_B;

    case SDLK_LALT:
      return BUTTON_A;

    default:
      return BUTTON_NONE;
  }
}
#endif
#if defined(PC_BUILD) || defined(RPI_BUILD)

u32 joy_map(u32 button)
{
  switch(button)
  {
    case 4:
      return BUTTON_L;

    case 5:
      return BUTTON_R;

    case 2:
      return BUTTON_START;

    case 3:
      return BUTTON_SELECT;

    case 1:
      return BUTTON_B;

    case 0:
      return BUTTON_A;

    default:
      return BUTTON_NONE;
  }
}

gui_action_type get_gui_input()
{
  SDL_Event event;
  gui_action_type gui_action = CURSOR_NONE;

  delay_us(30000);

  while(SDL_PollEvent(&event))
  { 
    switch(event.type)
    {
      case SDL_QUIT:
        quit();

      case SDL_KEYDOWN:
      {
        switch(event.key.keysym.sym)
        {
          case SDLK_ESCAPE:
            gui_action = CURSOR_EXIT;
            break;

          case SDLK_DOWN:
            gui_action = CURSOR_DOWN;
            break;

          case SDLK_UP:
            gui_action = CURSOR_UP;
            break;

          case SDLK_LEFT:
            gui_action = CURSOR_LEFT;
            break;

          case SDLK_RIGHT:
            gui_action = CURSOR_RIGHT;
            break;

          case SDLK_RETURN:
            gui_action = CURSOR_SELECT;
            break;

          case SDLK_BACKSPACE:
            gui_action = CURSOR_BACK;
            break;
	 default:
	    break;
      }
    }
    break;
#ifdef RPI_BUILD
    case SDL_JOYBUTTONDOWN:
    {
      switch (event.jbutton.button)
      {
	case 2:
            gui_action = CURSOR_BACK;
            break;

	case 1:
            gui_action = CURSOR_EXIT;
            break;

	case 0:
	    gui_action = CURSOR_SELECT;
    	    break;
	}
     }
     break;

     case SDL_JOYAXISMOTION:
     {
         if (event.jaxis.axis==0) { //Left-Right
            if (event.jaxis.value < -3200) gui_action = CURSOR_LEFT;
        	else if (event.jaxis.value > 3200) gui_action = CURSOR_RIGHT;
         }
         if (event.jaxis.axis==1) {  //Up-Down
            if (event.jaxis.value < -3200) gui_action = CURSOR_UP;
        	else if (event.jaxis.value > 3200) gui_action = CURSOR_DOWN;
         }
    }
    break;

#endif
    default:
        break;
    }
  }
  return gui_action;
}

u32 update_input()
{
  SDL_Event event;

  while(SDL_PollEvent(&event))
  {
    switch(event.type)
    {
      case SDL_QUIT:
        quit();

      case SDL_KEYDOWN:
      {
        if(event.key.keysym.sym == SDLK_ESCAPE)
        {
          quit();
        }
#ifdef PC_BUILD
        if(event.key.keysym.sym == SDLK_BACKSPACE)
#else
        if(event.key.keysym.sym == SDLK_F10)
#endif
        {
          u16 *screen_copy = copy_screen();
          u32 ret_val = menu(screen_copy);
          free(screen_copy);

          return ret_val;
        }
        else
#ifdef PC_BUILD
        if(event.key.keysym.sym == SDLK_F1)
        {
          debug_on();
        }
        else

        if(event.key.keysym.sym == SDLK_F2)
        {
          FILE *fp = fopen("palette_ram.bin", "wb");
          printf("writing palette RAM\n");
          fwrite(palette_ram, 1024, 1, fp);
          fclose(fp);
          printf("writing palette VRAM\n");
          fp = fopen("vram.bin", "wb");
          fwrite(vram, 1024 * 96, 1, fp);
          fclose(fp);
          printf("writing palette OAM RAM\n");
          fp = fopen("oam_ram.bin", "wb");
          fwrite(oam_ram, 1024, 1, fp);
          fclose(fp);
          printf("writing palette I/O registers\n");
          fp = fopen("io_registers.bin", "wb");
          fwrite(io_registers, 1024, 1, fp);
          fclose(fp);
        }
        else

        if(event.key.keysym.sym == SDLK_F3)
        {
          dump_translation_cache();
        }
        else
#endif
        if(event.key.keysym.sym == SDLK_F5)
        {
          char current_savestate_filename[512];
          u16 *current_screen = copy_screen();
          get_savestate_filename_noshot(savestate_slot,
           current_savestate_filename);
          save_state(current_savestate_filename, current_screen);
          free(current_screen);
        }
        else

        if(event.key.keysym.sym == SDLK_F7)
        {
          char current_savestate_filename[512];
          get_savestate_filename_noshot(savestate_slot,
           current_savestate_filename);
          load_state(current_savestate_filename);
          debug_on();
          return 1;
        }
        else

        if(event.key.keysym.sym == SDLK_BACKQUOTE)
        {
          synchronize_flag ^= 1;
        }
        else
        {
          key |= key_map(event.key.keysym.sym);
          trigger_key(key);
        }

        break;
      }

      case SDL_KEYUP:
      {
        key &= ~(key_map(event.key.keysym.sym));
        break;
      }

      case SDL_JOYBUTTONDOWN:
      {
        key |= joy_map(event.jbutton.button);
        trigger_key(key);
        break;
      }

      case SDL_JOYBUTTONUP:
      {
        key &= ~(joy_map(event.jbutton.button));
        break;
      }
#ifdef RPI_BUILD
      case SDL_JOYAXISMOTION:
      {
         if (event.jaxis.axis==0) { //Left-Right
            key &= ~(BUTTON_LEFT|BUTTON_RIGHT);
         if (event.jaxis.value < -3200)  key |= BUTTON_LEFT;
           else if (event.jaxis.value > 3200)  key |= BUTTON_RIGHT;
       } 
         if (event.jaxis.axis==1) {  //Up-Down
            key &= ~(BUTTON_UP|BUTTON_DOWN);
         if (event.jaxis.value < -3200)  key |= BUTTON_UP;
           else if (event.jaxis.value > 3200)  key |= BUTTON_DOWN;
       }
       break;
#endif
      }
    }
  }

  io_registers[REG_P1] = (~key) & 0x3FF;

  return 0;
}

void init_input()
{
  u32 joystick_count = SDL_NumJoysticks();

  if(joystick_count > 0)
  {
    SDL_JoystickOpen(0);
    SDL_JoystickEventState(SDL_ENABLE);
  }
}

#endif


#define input_savestate_builder(type)                                         \
void input_##type##_savestate(file_tag_type savestate_file)                   \
{                                                                             \
  file_##type##_variable(savestate_file, key);                                \
}                                                                             \

input_savestate_builder(read);
input_savestate_builder(write_mem);

//added by nirvous
int poxOpenInput()
{
	char dev[20];
	char name[32];
	int i;

	if(pox_evdev >= 0) {
		perror("pox_evdev already open");
		return 0;
	}

	for(i = 0; i < MAX_DEVNODES; i++) {
		sprintf(dev, "/dev/input/event%d", i);
		pox_evdev = open(dev, O_RDONLY);
		if(pox_evdev < 0) {
			printf("can't open %s\n", dev);
			return -1;
		}

		if(ioctl(pox_evdev, EVIOCGNAME(32), name) < 0) {
			perror("can't get input device name\n");
			close(pox_evdev);
			return -1;
		}

		if(!strcmp(name, "LF1000 Keyboard")) {
			printf("found keyboard on %s\n", dev);
			return pox_evdev;
		}
		else { /* not what we want, check another */
			close(pox_evdev);
			pox_evdev = -1;
		}
	}

	return -1;
}

//added by nirvous
void poxCloseInput() {
	if(pox_evdev >= 0) {
		close(pox_evdev);
		pox_evdev = -1;
	}
}

//added by nirvous
int poxReadInput(poxInputEvent *pie) {
	struct pollfd fds[1];

	if(!pie) return 0;
	if(pox_evdev < 0) {
		return 0;
	}

	fds[0].fd = pox_evdev;
	fds[0].events = POLLIN;

	if(poll(fds, 1, 0) == -1) {
		perror("can't poll evdev");
		return 0;
	}

	// Capture All events
	if(fds[0].revents & POLLIN) {
		struct input_event ev[1];

		int rd = read(pox_evdev, ev, sizeof(struct input_event));
		if(rd < (int) sizeof(struct input_event)) {
			perror("read error");
			return 0;
		}

		pie->code = ev[0].code;
		if(ev[0].value == 0) {
			pie->type = POX_KEYUP;
			return 1;
		} else
		if(ev[0].value == 1) {
			pie->type = POX_KEYDOWN;
			return 1;
		}
		return 0;
	}

	return 0;
}
