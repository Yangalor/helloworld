%%%-------------------------------------------------------------------
%% @doc helloworld gui.
%% @end
%%%-------------------------------------------------------------------

-module(helloworld_gui).

-include_lib("wx/include/wx.hrl").

-behavior(wx_object).


%% API

-export([start/0, start/1, start_link/0, start_link/1, 
		 init/1, handle_call/3, handle_cast/2, handle_event/2,
		 handle_info/2, code_change/3, terminate/2]).
		 
           
%% API

start() ->
    start([]).

start(Debug) ->
    wx_object:start(?MODULE, Debug, []).

start_link() ->
    start_link([]).

start_link(Debug) ->
    wx_object:start_link(?MODULE, Debug, []).

%% Callbacks

init(Args) ->
	
	Wx = wx:new(Args),	
	
	process_flag(trap_exit, true),
	
	Frame = wxFrame:new(Wx, ?wxID_ANY, "Привет, Мир!", [{size, {800, 600}}]),
	
	StatusBar = wxFrame:createStatusBar(Frame, []),	
	wxStatusBar:setFieldsCount(StatusBar, 3, [{widths, [-1, -1, 100]}]),	
	wxStatusBar:setStatusStyles(StatusBar, [?wxSB_FLAT, ?wxSB_FLAT, ?wxSB_NORMAL]),
	
	wxStatusBar:pushStatusText(StatusBar, io_lib:format("Время ~2.10.0b:~2.10.0b:~2.10.0b", tuple_to_list(time())), [{number, 2}]),
		
	wxFrame:show(Frame),
	
	{Frame, {}}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
	
terminate(_Reason, _State) ->
	wx:destroy().

handle_info({'EXIT', _, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT', _, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT', _, normal}, State) ->
    {noreply,State};
handle_info(_Msg, State) ->    
    {noreply,State}.
	
handle_call(_Msg, _From, State) ->    
    {reply,ok,State}.	

handle_cast(_Msg, State) ->    
    {noreply,State}.

handle_event(#wx{event=#wxClose{}}, State) ->        
    {stop, normal, State};
handle_event(_,State) ->    
    {noreply, State}.           

%% Internals
