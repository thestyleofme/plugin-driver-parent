package com.github.codingdebugallday.driver.common.infra.page;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;


/**
 * <p>
 * 分页参数接收
 * </p>
 *
 * @author JupiterMouse 2020/07/22
 * @since 1.0
 */
@Data
@AllArgsConstructor
public class PageRequestImpl {
    private int page;
    private int size;
    private Sort sort = Sort.unsorted();

    public PageRequestImpl() {

    }

    public Boolean paged() {
        if (size == 0) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    public PageRequest convert() {
        return PageRequest.of(page, size, sort);
    }

}
