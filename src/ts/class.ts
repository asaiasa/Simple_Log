import { commonFunction as CF } from './common';

/**
 * MINUTES class
 */
export class MINUTES {

    // common contents, single input
    private date: { [s: string]: string };  // year, month, date
    private member: string[];

    // multiple inputs
    private agenda: string[];
    private content: string[];
    private conclusion: string[];

    // option contents.
    private reason: string[];
    private memo: string[];
    private completed: (boolean | null)[];

    // inside variables.
    private id: string;

    constructor(member: string[])
    constructor(member: string[], date: { [s: string]: string }, id: string)
    constructor(member: string[], date: { [s: string]: string }, id: string, agenda: string[], content: string[], conclusion: string[], reason: string[], memo: string[], completed: (boolean | null)[])
    constructor(member: string[], date?: { [s: string]: string }, id?: string, agenda?: string[], content?: string[], conclusion?: string[], reason?: string[], memo?: string[], completed?: (boolean | null)[]) {

        // title
        if (date === undefined) this.date = this.getToday();
        else this.date = date;

        // members
        this.member = member;

        // agenda
        if (agenda === undefined) this.agenda = [];
        else this.agenda = agenda;

        // content
        if (content === undefined) this.content = [];
        else this.content = content

        // conclusion
        if (conclusion === undefined) this.conclusion = [];
        else this.conclusion = conclusion;

        // reason
        if (reason === undefined) this.reason = []
        else this.reason = reason;

        // memo
        if (memo === undefined) this.memo = [];
        else this.memo = memo;

        // completed
        if (completed === undefined) this.completed = [null];
        else this.completed = completed;

        // id
        if (id === undefined) this.id = this.generateId();
        else this.id = id;

    }

    /**
     * return today
     */
    private getToday = (): { [s: string]: string } => {
        var today: { [s: string]: string };
        var date: Date = new Date();

        today = {
            year: date.getFullYear().toString(),
            month: this.zeroPadding(date.getMonth() + 1),
            date: this.zeroPadding(date.getDate()),
        }

        return today;
    }

    /**
     * id is given by date and time
     */
    private generateId = (): string => {

        var date: Date = new Date();

        var id: string = "";

        id += date.getFullYear().toString();
        id += this.zeroPadding(date.getMonth() + 1);
        id += this.zeroPadding(date.getDate());
        id += this.zeroPadding(date.getHours());
        id += this.zeroPadding(date.getMinutes());
        id += this.zeroPadding(date.getSeconds());

        return id;
    }

    /**
     * zero padding
     */
    private zeroPadding = (num: number): string => { return (Array(2).join('0') + num).slice(-2); }

    /**
     * getter
     */
    public get_date = (): { [s: string]: string } => { return this.date; }
    public get_agenda = (index: number): string => { return this.agenda[index]; }
    public get_content = (index: number): string => { return this.content[index]; }
    public get_conclusion = (index: number): string => { return this.conclusion[index]; }
    public get_reason = (index: number): string => { return this.reason[index]; }
    public get_memo = (index: number): string => { return this.memo[index]; }
    public get_comp = (index: number): (boolean | null) => { return this.completed[index]; }
    public get_id = (): string => { return this.id; }

    /**
     * special getter
     */
    public get_date_s = (char: string): string => { return this.date['year'] + char + this.date['month'] + char + this.date['date']; }
    public get_date_s_as_num = (char: string): string => { return this.date['year'] + char + CF.trans_Month(this.date['month']) + char + this.date['date']; }

    /**
     * getter(all data)
     */
    public get_members = (): string[] => { return this.member; }
    public get_agendas = (): string[] => { return this.agenda; }
    public get_contents = (): string[] => { return this.content; }
    public get_conclusions = (): string[] => { return this.conclusion; }
    public get_reasons = (): string[] => { return this.reason; }
    public get_memos = (): string[] => { return this.memo; }
    public get_comps = (): (boolean | null)[] => { return this.completed; }

    /**
     * setter
     */
    public set_agenda = (index: number, agenda: string): void => { this.agenda[index] = agenda; }
    public set_content = (index: number, content: string): void => { this.content[index] = content; }
    public set_conclusion = (index: number, conclusion: string): void => { this.conclusion[index] = conclusion; }
    public set_reason = (index: number, reason: string): void => { this.reason[index] = reason; }
    public set_memo = (index: number, memo: string): void => { this.memo[index] = memo; }
    public set_comp = (index: number, comp: boolean | null): void => { this.completed[index] = comp; }
    public add_data = (agenda: string, content: string, conclusion: string, reason: string, memo: string, comp: boolean | null): void => {
        this.agenda.push(agenda);
        this.content.push(content);
        this.conclusion.push(conclusion);
        this.reason.push(reason);
        this.memo.push(memo);
        this.completed.push(comp);
    }
    public change_data = (index: number, agenda: string, content: string, conclusion: string, reason: string, memo: string, comp: boolean | null): void => {
        this.agenda[index] = agenda;
        this.content[index] = content;
        this.conclusion[index] = conclusion;
        this.reason[index] = reason;
        this.memo[index] = memo;
        this.completed[index] = comp;
    }

    /**
     * setter(all data)
     */
    public set_members = (member: string[]): void => { this.member = member; }
    public set_agendas = (agenda: string[]): void => { this.agenda = agenda; }
    public set_contents = (content: string[]): void => { this.content = content; }
    public set_conclusions = (conclusion: string[]): void => { this.conclusion = conclusion; }
    public set_reasons = (reason: string[]): void => { this.reason = reason; }
    public set_memos = (memo: string[]): void => { this.memo = memo; }
    public set_comps = (comp: (boolean | null)[]): void => { this.completed = comp; }

}

/**
 * NEXT AGENDA class
 */
export class NEXT_AGENDA {

    private name: string;
    private date: string;
    private id: string;

    constructor(name: string, date: string)
    constructor(name: string, date: string, id: string)
    constructor(name: string, date: string, id?: string) {
        this.name = name;
        this.date = date;
        if (id === undefined) this.id = this.generateId();
        else this.id = id;
    }

    /**
     * id is given by date and time
     */
    private generateId = (): string => {

        var date: Date = new Date();

        var id: string = "";

        id += date.getFullYear().toString();
        id += this.zeroPadding(date.getMonth() + 1);
        id += this.zeroPadding(date.getDate());
        id += this.zeroPadding(date.getHours());
        id += this.zeroPadding(date.getMinutes());
        id += this.zeroPadding(date.getSeconds());

        return id;
    }

    /**
     * zero padding
     */
    private zeroPadding = (num: number): string => { return (Array(2).join('0') + num).slice(-2); }

    /**
     * getter
     */
    public get_name = (): string => { return this.name; }
    public get_date = (): string => { return this.date; }
    public get_id = (): string => { return this.id; }

}

import { view_ddl_id } from './id';
/**
 * VIEW_DIALOG class
 */
export class VIEW_DIALOG {

    private dialog: HTMLDialogElement;

    constructor() {
        this.dialog = document.getElementById(view_ddl_id['dialogId']) as HTMLDialogElement;
        CF.addEvent(view_ddl_id['closeId'], (): void => { this.dialog.close(); });
    }

    /**
     * set data
     */
    public rendererView = (minutes: MINUTES): void => {
        if (!this.dialog.open) this.dialog.showModal();
        var obj: HTMLElement | null;

        // date
        obj = document.getElementById(view_ddl_id['date']);
        if (obj != null) obj.innerText = minutes.get_date_s('/');

        // member
        obj = document.getElementById(view_ddl_id['members']);
        if (obj != null) obj.innerText = this.connect_withchar(minutes.get_members(), ', ');

        // agenda
        obj = document.getElementById(view_ddl_id['agenda']);
        if (obj != null) obj.innerHTML = this.conenct_tolist(minutes.get_agendas());

        // contents
        obj = document.getElementById(view_ddl_id['contents']);
        if (obj != null) obj.innerHTML = this.conenct_tolist(minutes.get_contents());

        // conclusion
        obj = document.getElementById(view_ddl_id['conclusion']);
        if (obj != null) obj.innerHTML = this.conenct_tolist(minutes.get_conclusions());

        // reason
        obj = document.getElementById(view_ddl_id['reason']);
        if (obj != null) obj.innerHTML = this.conenct_tolist(minutes.get_reasons());

        // memo
        obj = document.getElementById(view_ddl_id['memo']);
        if (obj != null) obj.innerHTML = this.conenct_tolist(minutes.get_memos());

        // status
        obj = document.getElementById(view_ddl_id['status']);
        if (obj != null) obj.innerHTML = this.change_StatusToString(minutes.get_comps());

    }

    /**
     * transform string array to string 1
     */
    private connect_withchar = (str: string[], char: string): string => {
        var uni_str: string = str[0];
        const len: number = str.length;
        for (let i: number = 1; i < len; ++i)
            uni_str += (char + str[i]);
        return uni_str;
    }

    /**
     * transform string array to string 2
     */
    private conenct_tolist = (str: string[]): string => {
        var html_str: string = "<ol>";
        const len: number = str.length;
        for (let i: number = 0; i < len; ++i)
            html_str += ('<li>' + str[i] + '</li>');
        html_str += "</ol>";
        return html_str;
    }

    /**
     * transform boolean to string
     */
    private change_StatusToString = (TorF: (boolean | null)[]): string => {
        var html_str: string = "<ol>";
        const len: number = TorF.length;
        for (let i: number = 0; i < len; ++i) {
            if (TorF[i] == null) html_str += ('<li>' + view_ddl_id['status_null'] + '</li>');
            else if (TorF[i]) html_str += ('<li>' + view_ddl_id['status_true'] + '</li>');
            else if (!TorF[i]) html_str += ('<li>' + view_ddl_id['status_false'] + '</li>');
            else html_str += ('<li>' + view_ddl_id['status_null'] + '</li>');
        }
        html_str += "</ol>";
        return html_str;
    }

}

import { search_ddl_id } from './id';
/**
 * SEARCH_DIALOG class
 */
export class SEARCH_DIALOG {

    private dialog: HTMLDialogElement;
    private res_htmlId: string; // set search conditions as string to hideen html tag
    private do_reload: boolean;

    constructor(hideen_HTML_id: string) {
        this.dialog = document.getElementById(search_ddl_id['dialogId']) as HTMLDialogElement;
        this.res_htmlId = hideen_HTML_id;
        this.do_reload = false;
        this.addEvent();
    }

    /**
     * add event to modal window
     */
    private addEvent = (): void => {
        CF.addEvent(search_ddl_id['cancelId'], (): void => { this.dialog.close(); });
        CF.addEvent(search_ddl_id['checkboxId'], this.change_display, 'change');
        CF.addEvent(search_ddl_id['okId'], this.start_Search);
        this.change_display();
        this.dialog.addEventListener('close', this.dClose);
    }

    /**
     * if checkbox is checked, hide calendar.
     */
    private change_display = (): void => {
        var calendar: HTMLElement | null = document.getElementById(search_ddl_id['calendarId']);
        var checked: boolean = CF.getCheckboxStatus(search_ddl_id['checkboxId']);    // false means all date
        if (calendar != null) {
            if (checked) calendar.style.display = 'block';
            else calendar.style.display = 'none';
        } else console.log('get id error');
    }

    /**
     * get search conditions and set them hidden tag
     * conditions are hidden as string: text-condition, start-date-condition, end-date-condition
     * date-condition:ex)20201225
     */
    private start_Search = (): void => {
        var search_text: string = CF.getText(search_ddl_id['textId']);
        var checked: boolean = CF.getCheckboxStatus(search_ddl_id['checkboxId']);
        var date_range: string[] = CF.getCalendarDateRange('', 'num');

        if (search_text == "" && !checked) {
            // alert('Enter search conditions.')
            return;
        } else if (search_text == "" && checked) {
            if (date_range[0] == "" && date_range[1] == "") {
                // alert('Enter search conditions.')
                return;
            }
        }
        var conditions: string = search_text + ';' + date_range[0] + ';' + date_range[1];
        var hidden_tag: HTMLElement | null = document.getElementById(this.res_htmlId);
        if (hidden_tag == null) {
            console.log('no hidden tag');
            return;
        }
        else hidden_tag.innerText = conditions;
        this.do_reload = true;
        this.dialog.close();
    }

    /**
     * show modal window
     */
    public dialogOpen = (): void => {
        if (!this.dialog.open) this.dialog.showModal();
    }

    /**
     * closing process
     */
    private dClose = (): void => {
        if (this.do_reload) {
            this.do_reload = false;
            window.location.reload();
        }
    }

}
