export namespace commonFunction {

    /**
     * add event to html element.
     * ex button
     */
    export const addEvent = (id: string, function_name: { (): void }, action?: string): void => {
        const act: string = action === undefined ? 'click' : action;
        document.getElementById(id)?.addEventListener(act, (): void => { function_name(); });
    }

    /**
     * add event to html input element
     * ex: checkbox
     */
    export const addEvent_i = (id: string, function_name: { (): void }, action?: string): void => {
        const act: string = action === undefined ? 'click' : action;
        (document.getElementById(id) as HTMLInputElement)?.addEventListener(act, (): void => { function_name(); });
    }

    /**
     * get entered text in text-area
     */
    export const getText = (id: string): string => { return ((document.getElementById(id)) as HTMLTextAreaElement).value; }

    /**
     * set text to text-area
     */
    export const setText = (id: string, value: string): void => { ((document.getElementById(id)) as HTMLTextAreaElement).value = value }

    /**
     * get checkbox status
     */
    export const getCheckboxStatus = (id: string): boolean => { return (document.getElementById(id) as HTMLInputElement).checked; }

    /**
     * set checkbox status
     */
    export const setCheckboxStatus = (id: string, value: boolean): void => { (document.getElementById(id) as HTMLInputElement).checked = value; }

    /**
     * get selecterbox value/index
     */
    export const getSelecterboxValue = (id: string): string => { return (document.getElementById(id) as HTMLSelectElement).value; }
    export const addSelecterboxOption = (id: string, value: string) => { (document.getElementById(id) as HTMLSelectElement).add(new Option(value)); }

    /**
     * get date from calendar
     * default format is 'year/Month(En,3char)/date, ex:2020/Dec/25
     * you can select '/' , '.' and others by first argument. 
     * also you can get month as number if you set 'num' to second argument.
     */
    export const getCalendarDate = (connect_char: string = '/', month: string = 'en'): string => {
        var dp_year: Element = document.getElementsByClassName('year-text')[0] as Element;
        var dp_date: Element = document.getElementsByClassName('date-text')[0] as Element;
        var date: string[] = [];

        // year
        date.push(dp_year.innerHTML);

        // month
        var temp: string = dp_date.innerHTML.slice(5, 8);
        date.push(month == 'num' ? trans_Month(temp) : temp);

        // day
        date.push(dp_date.innerHTML.slice(9, 11));

        return date[0] + connect_char + date[1] + connect_char + date[2];
    }

    /**
     * get date rage from calendar.
     * [0] is start date, [1] is end date.
     * default format is 'year/Month(En,3char)/date, ex:2020/Dec/25
     * you can select '/' , '.' and others by first argument. 
     * also you can get month as number if you set 'num' to second argument.
     */
    export const getCalendarDateRange = (connect_char: string = '/', month: string = 'en'): string[] => {
        var dp_year: HTMLCollectionOf<Element> = document.getElementsByClassName('year-text');
        var dp_date: HTMLCollectionOf<Element> = document.getElementsByClassName('date-text');

        var start_date: string[] = [];
        var end_date: string[] = [];
        var date_range: string[] = [];

        // year
        start_date.push(dp_year[0].innerHTML);
        end_date.push(dp_year[1].innerHTML);

        // month
        var temp: string = dp_date[0].innerHTML.slice(5, 8);
        start_date.push(month == 'num' ? trans_Month(temp) : temp);

        temp = dp_date[1].innerHTML.slice(5, 8);
        end_date.push(month == 'num' ? trans_Month(temp) : temp);

        // day
        start_date.push(dp_date[0].innerHTML.slice(9, 11));
        end_date.push(dp_date[1].innerHTML.slice(9, 11));

        date_range.push(start_date[0] + connect_char + start_date[1] + connect_char + start_date[2]);
        date_range.push(end_date[0] + connect_char + end_date[1] + connect_char + end_date[2]);

        return date_range;
    }

    /**
     * translate month: string --> number
     */
    export const trans_Month = (str: string): string => {

        var month: string = "";
        switch (str) {
            case 'Jan':
                month = '1';
                break;
            case 'Feb':
                month = '2';
                break;
            case 'Mar':
                month = '3';
                break;
            case 'Apr':
                month = '4';
                break;
            case 'May':
                month = '5';
                break;
            case 'Jun':
                month = '6';
                break;
            case 'Jul':
                month = '7';
                break;
            case 'Aug':
                month = '8';
                break;
            case 'Sep':
                month = '9';
                break;
            case 'Oct':
                month = '10';
                break;
            case 'Nov':
                month = '11';
                break;
            case 'Dec':
                month = '12';
                break;
            default:
                break;
        }
        return month;
    }

}
