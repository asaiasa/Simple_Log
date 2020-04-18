import { MINUTES, NEXT_AGENDA } from './class';
import { commonFunction as CF } from './common';
import DM from './dataManager';
import { register_id } from './id';

const dm: DM = new DM();
var minutes: MINUTES[] = [];
var agenda_list: NEXT_AGENDA[] = [];
var new_minutes: MINUTES;
var current_page_index: number = -1;

window.onload = (): void => {
    addEvent();
    data_Init();
    selecter_Init();
}

/**
 * event listener
 */
const addEvent = (): void => {
    CF.addEvent(register_id['back_btn'], back_page);
    CF.addEvent(register_id['next_btn'], reload_page);
    CF.addEvent(register_id['register_btn'], register_all);
    CF.addEvent_i(register_id['agenda_checkbox'], change_display, 'change');
}

/**
 * get initial data from electron store
 */
const data_Init = (): void => {
    var temp: MINUTES | null;

    if ((temp = dm.get_Temp()) == null) {
        current_page_index = 1;
        temp = dm.get_New();
        if (temp == null) {
            console.log('initialize error!\nmove previous page.');
            location.href = '../html/minutes.html';
            return;
        }
    } else {
        current_page_index = Number(dm.get_free());
        if (current_page_index == 0) current_page_index = 1;
        if (temp.get_agendas().length < current_page_index) {
            // register new minutes page.
            // no action.
        } else {
            // already data are entered.
            // set them.
            set_PreviousData(temp);
        }
    }

    new_minutes = temp;
    minutes = dm.get_Minutes();
    agenda_list = dm.get_Agenda();

}

/**
 * Set data when pushed back button
 */
const set_PreviousData = (entered_data: MINUTES) => {

    var data_index: number = current_page_index - 1;
    var agenda: string = entered_data.get_agenda(data_index);
    var content: string = entered_data.get_content(data_index);
    var conclusion: string = entered_data.get_conclusion(data_index);
    var reason: string = entered_data.get_reason(data_index);
    var memo: string = entered_data.get_memo(data_index);

    CF.setText(register_id['agenda_input'], agenda);
    CF.setText(register_id['content_input'], content);
    CF.setText(register_id['conclusion_input'], conclusion);
    CF.setText(register_id['reason_input'], reason);
    CF.setText(register_id['memo_input'], memo);

    var obj: HTMLLabelElement | undefined;
    if (agenda != '') {
        obj = document.getElementById(register_id['agenda_input'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }
    if (content != '') {
        obj = document.getElementById(register_id['content_input'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }
    if (conclusion != '') {
        obj = document.getElementById(register_id['conclusion_input'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }
    if (reason != '') {
        obj = document.getElementById(register_id['reason_input'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }
    if (memo != '') {
        obj = document.getElementById(register_id['memo_input'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }

}

/**
 * insesrt data to select box and arrange display
 */
const selecter_Init = (): void => {
    agenda_list.forEach(value => CF.addSelecterboxOption(register_id['agenda_selecter'], value.get_name()));
    change_display();
}

/**
 * if checkbox is checked, hide calendar.
 */
const change_display = (): void => {
    var a_input: HTMLElement | null = document.getElementById(register_id['agenda_input']);
    var a_seslect: HTMLElement | null = document.getElementById(register_id['agenda_selecter_div']);
    var selecter_checked: boolean = CF.getCheckboxStatus(register_id['agenda_checkbox']);
    if (a_input != null && a_seslect != null) {
        if (selecter_checked) {
            a_input.style.display = 'none';
            a_seslect.style.display = 'block';
            let obj: HTMLLabelElement | undefined = document.getElementById(register_id['agenda_input'])?.parentElement?.getElementsByTagName('label')[0];
            if (obj !== undefined) obj.className = '';
        }
        else {
            a_input.style.display = 'block';
            a_seslect.style.display = 'none';
            if (CF.getText(register_id['agenda_input']) != '') {
                let obj: HTMLLabelElement | undefined = document.getElementById(register_id['agenda_input'])?.parentElement?.getElementsByTagName('label')[0];
                if (obj !== undefined) obj.className = 'active';
            }
        }
    }
}

/**
 * back to members page or previous data page
 */
const back_page = (): void => {
    if (1 < current_page_index) {
        dm.set_free((current_page_index - 1).toString());
        window.location.reload();
    }
    else {
        dm.get_New();
        dm.set_free('0');
        location.href = '../html/minutes.html';
    }
}

// get data and move next agenda
const reload_page = (): void => {
    if (get_entered_data(false)) {
        dm.set_Temp(new_minutes);
        dm.set_free((current_page_index + 1).toString());
        window.location.reload();
    }
}

/**
 * re-register agenda and minutes data, then back to home.
 */
const register_all = (): void => {
    if (get_entered_data(true)) {
        dm.set_Agenda(agenda_list);
        dm.set_Minutes(minutes);
        dm.del_New();
        dm.del_Temp();
        dm.del_free();
        location.href = '../html/index.html';
    }
}

/**
 * remove agenda data which used this time
 */
const pop_agenda_list = (list_selected: boolean, value: string): void => {
    if (!list_selected) return;
    const len: number = agenda_list.length;
    for (let i: number = 0; i < len; ++i) {
        if (agenda_list[i].get_name() == value) {
            agenda_list.splice(i, 1);
            break;
        }
    }
}

/**
 * get entered data. if data is correctly entered, return true.
 * @param registered - true when data input is completed(when register button is pushed)
 */
const get_entered_data = (registered: boolean): boolean => {

    var chkbox: boolean = CF.getCheckboxStatus(register_id['agenda_checkbox']);
    var select: string = CF.getSelecterboxValue(register_id['agenda_selecter']);
    var agenda: string = CF.getText(register_id['agenda_input']);
    var content: string = CF.getText(register_id['content_input']);
    var conclusion: string = CF.getText(register_id['conclusion_input']);

    if (entered_check(chkbox, select, agenda, content, conclusion)) {
        let reason: string = CF.getText(register_id['reason_input']);
        let memo: string = CF.getText(register_id['memo_input']);
        let comp: boolean | null = statusChanger(CF.getSelecterboxValue(register_id['status_selecter']));

        if (chkbox) agenda = select;
        push_data(registered, agenda, content, conclusion, reason, memo, comp);

        pop_agenda_list(chkbox, select);

        return true;
    } else {
        // alert('Enter text.');
        console.log('Enter text.');
        return false;
    }

}

/**
 * push to new minutes data
 * @param registered - true when data input is completed(when register button is pushed)
 * @param agenda - get from html
 * @param content - get from html
 * @param conc - get from html
 * @param reason - get from html
 * @param memo - get from html
 * @param comp - get from html
 */
const push_data = (registered: boolean, agenda: string, content: string, conc: string, reason: string, memo: string, comp: boolean | null): void => {
    // new_minutesに入力データを登録
    if (registered) {   // pushed register button
        new_minutes.add_data(agenda, content, conc, reason, memo, comp);
        minutes.push(new_minutes);
    } else {    // pushed next button
        if (new_minutes.get_agendas().length < current_page_index) {   // when store new data except when first data
            new_minutes.add_data(agenda, content, conc, reason, memo, comp);
        }
        else {  // when change existed data or first data
            new_minutes.change_data(current_page_index - 1, agenda, content, conc, reason, memo, comp);
        }
    }
}

/**
 * change status string to boolean or null
 * @param value - comp status value
 */
const statusChanger = (value: string): boolean | null => {
    return value == register_id['status_null'] ? null : (value == register_id['status_true'] ? true : false);
}

/**
 * check if the values are entered right
 * @param chkbox - checkbox whether use selecter or text input
 * @param selecter - value from agenda selecter
 * @param textinput - value of text input
 */
const entered_check = (chkbox: boolean, selecter: string, agenda: string, content: string, conc: string): boolean => {
    if (content == '' || conc == '') return false;
    return chkbox ? (selecter == '' ? false : true) : (agenda == '' ? false : true);
}
