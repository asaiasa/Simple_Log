import { MINUTES } from './class';
import { commonFunction as CF } from './common';
import { subcommon as SCF } from './subcommon';
import DM from './dataManager';
import { output_id } from './id';

/**
 * initialization
 */
window.onload = (): void => {
    CF.addEvent(output_id['ok_btn'], csv_export);
    CF.addEvent_i(output_id['check_btn'], change_display, 'change');
    change_display();
}

/**
 * if checkbox is checked, hide calendar.
 */
const change_display = (): void => {
    var textarea: HTMLElement | null = document.getElementById(output_id['textarea_id']);
    var calendar: HTMLElement | null = document.getElementById(output_id['calendar_id']);
    var all_checked: boolean = CF.getCheckboxStatus(output_id['check_btn']);
    if (textarea != null && calendar != null) {
        if (all_checked) {
            textarea.style.display = 'none';
            calendar.style.display = 'none';
        }
        else {
            textarea.style.display = 'block';
            calendar.style.display = 'block';
        }
    }
}

/**
 * get minutes data satisfying search conditions
 */
const get_data = (): MINUTES[] | null => {

    var search_text: string = CF.getText(output_id['textarea_id']);
    var checked: boolean = CF.getCheckboxStatus(output_id['check_btn']);
    var date_range: string[] = CF.getCalendarDateRange('', 'num');

    let dm: DM = new DM();
    var minutes: MINUTES[] = dm.get_Minutes();
    if (checked) return minutes;

    if (search_text == '' && date_range[0] == '' && date_range[1] == '') {
        // alert('Enter search conditions.');
        return null;
    }

    var searched_index: number[] = SCF.search_Minutes(minutes, [search_text, date_range[0], date_range[1]]);
    var searched_mintues: MINUTES[] = [];
    searched_index.forEach(index => searched_mintues.push(minutes[index]));

    return searched_mintues;

}

/**
 * export(download) minutes as csv file
 */
const csv_export = (): void => {

    var data: MINUTES[] | null = get_data();

    if (data == null || data.length == 0) return;

    const csvContent: string = convert_DATA(data).map(e => e.join(';')).join('\n');
    const bom = new Uint8Array([0xEF, 0xBB, 0xBF]);
    const blob: Blob = new Blob([bom, csvContent], { type: 'text/csv;charset=utf-8;' });

    if (navigator.msSaveBlob) { // IE 10+
        navigator.msSaveBlob(blob, 'minutes.csv');
    } else {
        let link: HTMLAnchorElement = document.createElement('a');
        if (link.download !== undefined) { // feature detection
            // Browsers that support HTML5 download attribute
            let url: string = URL.createObjectURL(blob);
            link.setAttribute('href', url);
            link.setAttribute('download', 'minutes.csv');
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            // alert('Specify ";" as the delimiter when opening the CSV file.');
        }
    }
}

/**
 * convert minutes data to csv format
 * @param data - minutes class data
 */
const convert_DATA = (data: MINUTES[]): string[][] => {
    const header: string[] = ['index', 'Date', 'members', 'agenda', 'content', 'conclusion', 'reason', 'memo', 'status'];
    var converted_data: string[][] = [];
    var temp: string[] = [];
    converted_data.push(header);
    data.forEach((value, index) => {
        temp.push(index.toString());
        temp.push(value.get_date_s('.'));
        temp.push(value.get_members().toString());
        temp.push(value.get_agenda(0));
        temp.push(value.get_content(0));
        temp.push(value.get_conclusion(0));
        temp.push(value.get_reason(0));
        temp.push(value.get_memo(0));
        temp.push(get_status(value.get_comp(0)));
        converted_data.push(temp);

        const list_num: number = value.get_agendas().length;
        for (let i: number = 1; i < list_num; ++i) {
            temp = [];
            temp.push('');
            temp.push('');
            temp.push('');
            temp.push(value.get_agenda(i));
            temp.push(value.get_content(i));
            temp.push(value.get_conclusion(i));
            temp.push(value.get_reason(i));
            temp.push(value.get_memo(i));
            temp.push(get_status(value.get_comp(i)));
            converted_data.push(temp);
        }

    });
    return converted_data;
}

/**
 * change true for completed, false for incompleted
 * @param bool 
 */
const get_status = (bool: boolean | null): string => {
    return bool == null ? 'none' : (bool ? 'completed' : 'incompleted');
}