%%%-------------------------------------------------------------------
%% @doc helloworld gui.
%% @end
%%%-------------------------------------------------------------------

-module(helloworld_gui).

-include_lib("wx/include/wx.hrl").

-behavior(wx_object).


%% Блок объявления API

-export([start/0, start/1, start_link/0, start_link/1, 
		 init/1, handle_call/3, handle_cast/2, handle_event/2,
		 handle_info/2, code_change/3, terminate/2]).
		 
-export([timeupdate/0]).
		 
           
%% Блок реализации базового API

start() ->
    start([]).

start(Debug) ->
    wx_object:start({local, ?MODULE}, ?MODULE, Debug, []).

start_link() ->
    start_link([]).

start_link(Debug) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Debug, []).

%% Блок реализации API wx_object

init(Args) ->
	% Инициализация GUI
	Wx = wx:new(Args),	
	
	% Перехват сообщений от связанных процессов
	process_flag(trap_exit, true),
	
	% Создание главного окна приложения
	Frame = wxFrame:new(Wx, ?wxID_ANY, "Привет, Мир!", [{size, {800, 600}}]),
	
	% Создание и настройка статусной строки из трех секций
	StatusBar = wxFrame:createStatusBar(Frame, []),	
	wxStatusBar:setFieldsCount(StatusBar, 3, [{widths, [-1, -1, 100]}]),	
	wxStatusBar:setStatusStyles(StatusBar, [?wxSB_FLAT, ?wxSB_FLAT, ?wxSB_NORMAL]),
	
	% Запуск таймера обновления системного времени в статусной строке GUI
	timer:apply_interval(1000, ?MODULE, timeupdate, []),
	
	% Отображение главного окна приложения на экране	
	wxFrame:show(Frame),
	
	% Возврат главного окна приложения и статуса GUI
	{Frame, {Frame}}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
	
terminate(_Reason, _State) ->
	wx:destroy().

%% Блок обработки сообщений от процессов

handle_info({'EXIT', _, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT', _, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT', _, normal}, State) ->
    {noreply,State};
handle_info(_Msg, State) ->    
    {noreply,State}.
	
%% Блок обработки синхронных вызовов
	
handle_call(_Msg, _From, State) ->  			% Обработка прочих синхронных вызовов  
    {reply,ok,State}.	

%% Блок обратоки асинхронных вызовов

handle_cast(timeupdate, State = {Frame}) ->		% Обновление времени по таймеру в статусной строке GUI
	% Получение статусной строки GUI
	StatusBar = wxFrame:getStatusBar(Frame),	
	
	% Вывод отформатированного отображения времени в третью секцию статусной строки GUI
	wxStatusBar:pushStatusText(StatusBar, io_lib:format("Время ~2.10.0b:~2.10.0b:~2.10.0b", tuple_to_list(time())), [{number, 2}]),
	
	{noreply,State};
handle_cast(_Msg, State) ->    					% Обработка прочих асинхронных вызовов
    {noreply,State}.

handle_event(#wx{event=#wxClose{}}, State) ->	% Обработка события закрытия главного окна GUI
    {stop, normal, State};
handle_event(_,State) ->    					% Обработка прочих событий
    {noreply, State}.           

%% Блок внутренних процедур и функций

timeupdate() -> 								% Интерфейс вызова обновления времени в статусной строке GUI
	wx_object:cast(?MODULE, timeupdate).

