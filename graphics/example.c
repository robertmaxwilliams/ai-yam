#include <stdio.h>
#include <stdlib.h>
#define SDL_MAIN_HANDLED
#include <SDL.h>


volatile sig_atomic_t stop;
void inthand(int signum) {
    stop = 1;
}

SDL_Window *window;
SDL_Renderer *renderer;

int main(int argc, char* argv[])
{

    int breakmode = 0;
    int pxp = 1;
    int WIDTH = 900; ///640;
    int HEIGHT = 700; //480;
    char colormode = '0';

    SDL_Init(SDL_INIT_EVERYTHING);
    SDL_Event event;
    
    SDL_SetWindowResizable(window, SDL_TRUE);
    SDL_CreateWindowAndRenderer(WIDTH, HEIGHT, 0, &window, &renderer);
    SDL_SetWindowResizable(window, SDL_TRUE);
    SDL_RenderClear(renderer);
    signal(SIGINT, inthand);
 
    int quit = 0; // set to true by pressing esc
    int nomore = 0;
    int i = 0;

    SDL_SetRenderDrawColor(renderer, 0, 100, 100, 100);
    SDL_RenderClear(renderer);
    
    while (!(stop || quit))
    {
        while (SDL_PollEvent(&event))
        {
            switch(event.type) 
            {
                case SDL_QUIT:
                    quit = 1;
                    break;

                case SDL_MOUSEMOTION:
                    //....
                    break;

                case SDL_KEYDOWN:
                    switch(event.key.keysym.sym) 
                    {
                        case SDLK_ESCAPE: 
                            quit = 1;
                            break;
                            // cases for other keypresses
                    }
                    break;
                case SDL_WINDOWEVENT:
                    switch(event.window.event) 
                    {
                        case SDL_WINDOWEVENT_RESIZED:
                            printf("Window %d resized to %dx%d\n",
                                event.window.windowID, event.window.data1,
                                event.window.data2);
                            WIDTH = event.window.data1;
                            HEIGHT = event.window.data2;
                            break;
                        case SDL_WINDOWEVENT_SIZE_CHANGED:
                            printf("Window %d size changed to %dx%d\n",
                                event.window.windowID, event.window.data1,
                                event.window.data2);
                            WIDTH = event.window.data1;
                            HEIGHT = event.window.data2;
                            break;
                        case SDL_WINDOWEVENT_MAXIMIZED:
                            printf("Window %d maximized\n", event.window.windowID);
                            printf("Window %d size changed to %dx%d\n",
                                event.window.windowID, event.window.data1,
                                event.window.data2);
                            WIDTH = event.window.data1;
                            HEIGHT = event.window.data2;
                            break;
                    }
            }
        }

        SDL_Delay(50);
        if (nomore) continue;
        SDL_RenderClear(renderer);


        SDL_SetRenderDrawColor(renderer, 0, 100, 100, 100);
        SDL_RenderPresent(renderer);
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_SUCCESS;
}
