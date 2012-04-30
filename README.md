#echo-exchange

Реализация web-приложения "Биржа. Сделки по ценным бумагам". Тестовый сервер запушен на http://juise.ru:8888/

## Серверная часть

* Клонирование:

`git clone git://github.com/juise/echo-exchange.git`

* Запуск:

`./start.sh`

Сервер слушает порт 8888 на всех доступных сетевых интерфейсах (0.0.0.0). Логи web-сервера Yaws хранятся в каталоге /tmp.

## Клиентская часть

* Добавление сгенерированных случайным образом данных о сделках на сервер:

```
./client_add.sh - для работы с сервером запущенным на localhost:8888
./client_add.sh host:port - для работы с сервером запущенным на host:port
```

* Выборка данных с сервера по случайно сгенерированному критерию:

```
./client_select.sh - для работы с сервером запущенным на localhost:8888
./client_select.sh host:port - для работы с сервером запущенным на host:port
```

## API

Scale = minute | hour | day | week | month, (номер недели выводится в позиции дня месяца, например 2012-01-02T00:00:00 - вторая неделя, январь)

Time = Y-M-DTh:m:s, (2012-11-28T16:54:56)

```
http://host:port/api/v1/all - Выборка всех данных с сервера
http://host:port/api/v1/all/Scale - Выборка всех данных с сервера с заданным масштабом

http://host:port/api/v1/Name - Выборка всех данных с сервера по названию ценной бумаги
http://host:port/api/v1/Name/Scale - Выборка всех данных с сервера по названию ценной бумаги с заданныи масштабом

http://host:port/api/v1/Time1/Time2 - Выборка всех данных с сервера за заданный временной диапазон
http://host:port/api/v1/Time1/Time2/Scale - Выборка всех данных с сервера за заданный временной диапазон с заданным масштабом

http://host:port/api/v1/Name/Time1/Time2 - Выборка всех данных с сервера по названию ценной бумаги за заданный временной диапазон
http://host:port/api/v1/Name/Time1/Time2/Scale - Выборка всех данных с сервера по названию ценной бумаги за заданный временной диапазон с заданным масштабом

http://host:port/api/v1/add - Добавление данных, поля учавствующие в POST-запросе - name, time, price, value

```

Например:

```
http://juise.ru:8888/api/v1/all/
http://juise.ru:8888/api/v1/ECHO/minute

http://juise.ru:8888/api/v1/ECHO/
http://juise.ru:8888/api/v1/ECHO/hour

http://juise.ru:8888/api/v1/2012-01-25T08:00:00/2012-11-25T14:20:00
http://juise.ru:8888/api/v1/2012-01-25T08:00:00/2012-11-25T14:20:00/day

http://juise.ru:8888/api/v1/YNDX/2012-01-25T08:00:00/2012-11-25T14:20:00/
http://juise.ru:8888/api/v1/YNDX/2012-01-25T08:00:00/2012-11-25T14:20:00/month

curl http://juise.ru:8888/api/v1/add -d "name=ECHO" -d "time=2012-07-04T03:25:53" -d "price=150" -d "value=200"
```