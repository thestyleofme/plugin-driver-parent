package com.github.codingdebugallday.driver.core.infra.utils;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nullable;

import com.google.common.collect.ImmutableList;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.util.Assert;

/**
 * <p>
 * 后端LIST分页
 * </p>
 *
 * @author JupiterMouse 2020/07/22
 * @since 1.0
 */
public final class PageUtil {

    private PageUtil() throws IllegalAccessException {
        throw new IllegalAccessException("util class");
    }

    /**
     * 分页数据
     *
     * @param elements    原始数据
     * @param pageRequest 分页对象
     * @param <T>         对象类型
     * @return LIST<T>
     */
    public static <T> List<T> paginate(final List<T> elements, @Nullable final PageRequest pageRequest) {
        final ImmutableList.Builder<T> results = ImmutableList.builder();
        Assert.notNull(pageRequest, "pageRequest is null");
        results.addAll(
                elements
                        .stream()
                        .skip((long) pageRequest.getPageNumber() * pageRequest.getPageSize())
                        .limit(pageRequest.getPageSize())
                        .collect(Collectors.toList())
        );
        return results.build();
    }

    /**
     * 分页处理
     *
     * @param elements    原始数据
     * @param pageRequest 分页对象
     * @param <T>         对象类型
     * @return Page<T>
     */
    public static <T> Page<T> doPage(final List<T> elements, @Nullable final PageRequest pageRequest) {
        Assert.notNull(pageRequest, "pageRequest is null");
        List<T> list = PageUtil.paginate(elements, pageRequest);
        return new PageImpl<>(list, pageRequest, elements.size());
    }

}
